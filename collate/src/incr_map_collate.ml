open! Core
module Collate_params = Collate_params
module Collated = Collated
module Store = Incr_memoize.Store
module Store_params = Incr_memoize.Store_params

module Compare = struct
  type ('k, 'v, 'cmp) t =
    | Unchanged
    | Reversed
    | Custom_by_value of { compare : 'v -> 'v -> int }
    | Custom_by_key_and_value of { compare : 'k * 'v -> 'k * 'v -> int }
  [@@deriving sexp_of]
end

module Custom_tuple_comparator = struct
  include%template Comparator.Derived2 [@mode portable] (struct
      type ('a, 'b) t = 'a * 'b

      let[@inline always] compare compare_k compare_v (k1, v1) (k2, v2) =
        let cmp_v = compare_v v1 v2 in
        if cmp_v <> 0 then cmp_v else compare_k k1 k2
      ;;

      let sexp_of_t = Tuple2.sexp_of_t
    end)
end

module Range_memoize_bucket = struct
  type key_bucket =
    | All_rows
    | Not_all_rows
  [@@deriving sexp_of, equal, hash, compare]

  type rank_bucket =
    | All_rows
    | From of int
    | To of int
    | Between of (int * int)
  [@@deriving sexp_of, equal, hash, compare]

  module T = struct
    type t = key_bucket * rank_bucket [@@deriving sexp_of, equal, hash, compare]
  end

  include T

  include%template Comparable.Make_plain [@mode portable] (T)

  let create
    ~bucket_size
    ~(key_range : _ Collate_params.Which_range.t)
    ~(rank_range : int Collate_params.Which_range.t)
    =
    let key_bucket : key_bucket =
      match key_range with
      | All_rows -> All_rows
      | From _ | To _ | Between _ -> Not_all_rows
    in
    let rank_bucket =
      match rank_range with
      | All_rows -> All_rows
      | From i -> From (i / bucket_size)
      | To i -> To (i / bucket_size)
      | Between (i, j) -> Between (i / bucket_size, j / bucket_size)
    in
    key_bucket, rank_bucket
  ;;
end

module Incr_collated_map = struct
  type ('k, 'v, 'cmp, 'w) t =
    | Original : (('k, 'v, 'cmp) Map.t, 'w) Incremental.t -> ('k, 'v, 'cmp, 'w) t
    | Sorted :
        ((('k * 'v, 'v, 'custom_cmp) Map.t, 'w) Incremental.t * ('k, 'cmp) Comparator.t)
        -> ('k, 'v, 'cmp, 'w) t

  let length t =
    let open Incremental.Let_syntax in
    match t with
    | Original m -> m >>| Map.length
    | Sorted (m, _) -> m >>| Map.length
  ;;

  module Key_rank = struct
    module Erased_tree = struct
      (* Ideally, this type would be

         {[
           type ('k, 'v) t : value mod contended portable with 'k with 'v =
             | T : ('k, 'v, _) Map.Tree.t -> ('k, 'v) t
         ]}

         (which is totally safe btw), but the compiler currently doesn't support mode
         crossing in GADTs. A feature is in the works, but until then, we're left with a
         little transmutation. *)

      type ('k, 'v) t : value mod contended portable with 'k with 'v

      let to_tree (type k v) : (k, v, _) Map.Tree.t -> (k, v) t = Obj.magic
      let of_tree (type k v) : (k, v) t -> (k, v, _) Map.Tree.t = Obj.magic
    end

    type ('k
         , 'v
         , 'cmp)
         t :
         value mod contended portable with 'k with 'v with ('k, 'cmp) Comparator.t =
      | Original of ('k, 'v, 'cmp) Map.t
      | Sorted of
          { tree : ('k * 'v, 'v) Erased_tree.t
          ; cmp : ('k, 'cmp) Comparator.t
          }

    let lookup t key =
      match t with
      | Original map -> Map.rank map key
      | Sorted { tree; cmp } ->
        let tree = Erased_tree.of_tree tree in
        let compare = Comparator.compare cmp in
        Map.Tree.fold_until
          tree
          ~init:0
          ~f:(fun ~key:(k, _) ~data:_ i ->
            if compare k key = 0 then Stop (Some i) else Continue (i + 1))
          ~finish:(fun _ -> None)
    ;;
  end

  let key_rank t =
    let open Incremental.Let_syntax in
    match t with
    | Original m ->
      let%map m in
      Key_rank.Original m
    | Sorted (m, key_comparator) ->
      let%map m in
      let tree = Key_rank.Erased_tree.to_tree (Map.to_tree m) in
      Key_rank.Sorted { tree; cmp = key_comparator }
  ;;
end

module Key_rank = Incr_collated_map.Key_rank

module Fold_params = struct
  type ('k, 'v, 'acc) t =
    { init : 'acc
    ; add : key:'k -> data:'v -> 'acc -> 'acc
    ; remove : key:'k -> data:'v -> 'acc -> 'acc
    ; update : (key:'k -> old_data:'v -> new_data:'v -> 'acc -> 'acc) option
    ; finalize : ('acc -> 'acc) option
    ; revert_to_init_when_empty : bool
    }
end

module Fold_action = struct
  type ('k, 'v, 'acc) t =
    | Fold : ('k, 'v, 'acc) Fold_params.t -> ('k, 'v, 'acc) t
    | Don't_fold : ('k, 'v, unit) t
end

module Fold = struct
  include Fold_params

  let create ?(revert_to_init_when_empty = true) ~init ~add ?update ~remove ?finalize () =
    { init; add; update; remove; finalize; revert_to_init_when_empty }
  ;;
end

open Incremental.Let_syntax

let do_filter ~instrumentation data ~predicate =
  match predicate with
  | None -> data
  | Some filter ->
    Incr_map.filter_mapi ?instrumentation data ~f:(fun ~key ~data ->
      if filter ~key ~data then Some data else None)
;;

let do_filter_sorted ~instrumentation (data : _ Incr_collated_map.t) ~predicate
  : _ Incr_collated_map.t
  =
  let filter
    (type a c)
    ~(get : a -> 'v -> 'k * 'v)
    (m : ((a, 'v, c) Map.t, 'w) Incremental.t)
    : ((a, 'v, c) Map.t, 'w) Incremental.t
    =
    match predicate with
    | None -> m
    | Some filter ->
      Incr_map.filter_mapi ?instrumentation m ~f:(fun ~key ~data ->
        let key, data = get key data in
        if filter ~key ~data then Some data else None)
  in
  match data with
  | Original m -> Original (filter ~get:(fun k v -> k, v) m)
  | Sorted (m, key_cmp) -> Sorted (filter ~get:(fun (k, _v1) v2 -> k, v2) m, key_cmp)
;;

let do_fold
  ~instrumentation
  (data : _ Incr_collated_map.t)
  ({ init; add; remove; update; finalize; revert_to_init_when_empty } : _ Fold_params.t)
  =
  match data with
  | Original map ->
    Incr_map.unordered_fold ?instrumentation map ~init ~add ~remove ?update
  | Sorted (map, _) ->
    let lift f ~key ~data acc =
      let key, _ = key in
      f ~key ~data acc
    in
    let update =
      Option.map update ~f:(fun update ~key ~old_data ~new_data acc ->
        let key, _ = key in
        update ~key ~old_data ~new_data acc)
    in
    Incr_map.unordered_fold
      ?instrumentation
      map
      ~init
      ~add:(fun ~key ~data acc -> lift add ~key ~data acc)
      ~remove:(fun ~key ~data acc -> lift remove ~key ~data acc)
      ~revert_to_init_when_empty
      ?update
      ?finalize
;;

let do_fold
  (type fold_result)
  (data : _ Incr_collated_map.t)
  ~instrumentation
  ~incremental_state
  ~in_scope
  ~(fold_action : (_, _, fold_result) Fold_action.t)
  =
  match fold_action with
  | Fold_action.Fold fold_params ->
    in_scope (fun () -> do_fold ~instrumentation data fold_params)
  | Don't_fold -> in_scope (fun () -> Incremental.return incremental_state ())
;;

let do_sort
  (type k v cmp custom_cmp w)
  ~instrumentation
  (data : ((k, v, cmp) Map.t, w) Incremental.t)
  ~(map_comparator : (k, cmp) Comparator.t)
  ~(custom_comparator : (k * v, custom_cmp) Comparator.t option)
  : (k, v, cmp, w) Incr_collated_map.t
  =
  match custom_comparator with
  | None -> Incr_collated_map.Original data
  | Some custom_comparator ->
    let sorted =
      Incr_map.unordered_fold
        ?instrumentation
        ~init:(Map.Using_comparator.empty ~comparator:custom_comparator)
        ~add:(fun ~key ~data map -> Map.set map ~key:(key, data) ~data)
        ~remove:(fun ~key ~data map -> Map.remove map (key, data))
        ~update:(fun ~key ~old_data ~new_data output ->
          let prev_key = key, old_data in
          let new_key = key, new_data in
          if (Comparator.compare custom_comparator) prev_key new_key = 0
          then Map.set output ~key:new_key ~data:new_data
          else Map.remove output prev_key |> Map.set ~key:new_key ~data:new_data)
        ~specialized_initial:(fun ~init new_in ->
          let[@inline always] compare_ignoring_second (t1, _) (t2, _) =
            (Comparator.compare custom_comparator) t1 t2
          in
          match Map.min_elt new_in with
          | None -> init
          | Some (any_key, any_data) ->
            let arr =
              Array.create ~len:(Map.length new_in) ((any_key, any_data), any_data)
            in
            let idx = ref 0 in
            Map.iteri new_in ~f:(fun ~key ~data ->
              arr.(!idx) <- (key, data), data;
              incr idx);
            Array.sort arr ~compare:compare_ignoring_second;
            Map.Using_comparator.of_sorted_array_unchecked
              ~comparator:custom_comparator
              arr)
        data
    in
    Sorted (sorted, map_comparator)
;;

module Which_range = struct
  let to_from_start_rank_range
    (rank_range : Collate_params.Rank.t Collate_params.Which_range.t)
    ~(data_length : int)
    : int Collate_params.Which_range.t
    =
    match rank_range with
    | All_rows -> All_rows
    | Between (l, u) ->
      let l = Rank_from_start.of_rank ~data_length l in
      let u = Rank_from_start.of_rank ~data_length u in
      Collate_params.Which_range.Between (l, u)
    | From l ->
      let l = Rank_from_start.of_rank ~data_length l in
      Collate_params.Which_range.From l
    | To u ->
      let u = Rank_from_start.of_rank ~data_length u in
      Collate_params.Which_range.To u
  ;;
end

let do_rank_range_restrict
  (type k v cmp w)
  ~subrange_by_rank_instrumentation
  (data : (k, v, cmp, w) Incr_collated_map.t)
  ~(rank_range : (Rank_from_start.Range.t, w) Incremental.t)
  : (k, v, cmp, w) Incr_collated_map.t
  =
  match data with
  | Original m ->
    let resolved_data =
      Incr_map.subrange_by_rank
        ?instrumentation:subrange_by_rank_instrumentation
        m
        rank_range
    in
    Original resolved_data
  | Sorted (m, key_cmp) ->
    let resolved_data =
      Incr_map.subrange_by_rank
        ?instrumentation:subrange_by_rank_instrumentation
        m
        rank_range
    in
    Sorted (resolved_data, key_cmp)
;;

type ('k, 'v) kv_custom_comparator =
  | T : ('k * 'v, _) Comparator.t option -> ('k, 'v) kv_custom_comparator

let comparator_of_compare
  (type k v cmp)
  ~(map_comparator : (k, cmp) Comparator.t)
  (compare : (k, v, cmp) Compare.t)
  : (k, v) kv_custom_comparator
  =
  match compare with
  | Unchanged -> T None
  | Reversed ->
    let (module Cmp : Comparator.S_fc with type comparable_t = k * v) =
      Comparator.make
        ~compare:(fun [@inline always] (k1, _v1) (k2, _v2) ->
          (Comparator.compare map_comparator) k2 k1)
        ~sexp_of_t:(fun (k, _v) -> (Comparator.sexp_of_t map_comparator) k)
    in
    T (Some Cmp.comparator)
  | Custom_by_value { compare } ->
    let (module Cmp) = Comparator.make ~compare ~sexp_of_t:(fun _v -> Sexp.Atom "") in
    let custom_comparator =
      Custom_tuple_comparator.comparator map_comparator Cmp.comparator
    in
    T (Some custom_comparator)
  | Custom_by_key_and_value { compare } ->
    let (module Cmp) =
      Comparator.make
        ~compare:(fun [@inline always] (k1, v1) (k2, v2) ->
          let res = compare (k1, v1) (k2, v2) in
          if res <> 0 then res else (Comparator.compare map_comparator) k1 k2)
        ~sexp_of_t:(fun (k, _v) -> (Comparator.sexp_of_t map_comparator) k)
    in
    T (Some Cmp.comparator)
;;

let with_cutoff incr ~equal =
  Incremental.set_cutoff incr (Incremental.Cutoff.of_equal equal);
  incr
;;

let do_to_pos_map (type k v cmp w) (data : (k, v, cmp, w) Incr_collated_map.t) =
  match data with
  | Original data ->
    Opaque_map.erase_key_incrementally data ~get:(fun ~key ~data -> key, data)
  | Sorted (data, _key_cmp) ->
    Opaque_map.erase_key_incrementally data ~get:(fun ~key:(k, _v1) ~data:v2 -> k, v2)
;;

type ('k, 'v, 'cmp, 'fold_result, 'w) t =
  { collated : (('k, 'v) Collated.t, 'w) Incremental.t
  ; key_rank : (('k, 'v, 'cmp) Incr_collated_map.Key_rank.t, 'w) Incremental.t
  ; fold_result : ('fold_result, 'w) Incremental.t
  }

let collated t = t.collated

let key_rank t =
  let%map key_rank = t.key_rank in
  fun key -> Incr_collated_map.Key_rank.lookup key_rank key
;;

let key_rank' t = t.key_rank
let fold_result t = t.fold_result

(** Converts a key range to a rank range.

    First adjusts the bounds of [which_key_range] by swapping out-of-dataset keys for
    their nearest in-dataset neighbors, and then calls
    [Rank_from_start.Rank.of_key_range]. *)
let which_key_range_to_rank_range_from_start
  (type k v cmp w)
  (data : (k, v, cmp, w) Incr_collated_map.t)
  ~(orig_map : ((k, v, cmp) Map.t, w) Incremental.t)
  ~(which_key_range : (k Collate_params.Which_range.t, w) Incremental.t)
  ~key_to_rank_instrumentation
  : (Rank_from_start.Range.t, w) Incremental.t
  =
  let incremental_state = Incremental.state orig_map in
  let to_bounds_pair ~lookup_lower_bound ~lookup_upper_bound which_range =
    match%pattern_bind which_range with
    | Collate_params.Which_range.All_rows ->
      Incremental.return incremental_state (Maybe_bound.Unbounded, Maybe_bound.Unbounded)
    | Between (l, u) ->
      let%mapn l = lookup_lower_bound l
      and u = lookup_upper_bound u in
      l, u
    | From l ->
      let%mapn l = lookup_lower_bound l in
      l, Maybe_bound.Unbounded
    | To u ->
      let%mapn u = lookup_upper_bound u in
      Maybe_bound.Unbounded, u
  in
  match data with
  | Original m ->
    let find_gte_key m k =
      match Map.closest_key m `Greater_or_equal_to k with
      | Some (k, _) -> Some k
      | None -> None
    in
    let find_lte_key m k =
      match Map.closest_key m `Less_or_equal_to k with
      | Some (k, _) -> Some k
      | None -> None
    in
    (* Transforms a key into a [Maybe_bound.t]. If the key isn't in the dataset, attempt
       to use the nearest key in the dataset. *)
    let lookup_lower_bound k =
      let%mapn m and k in
      match Map.find m k with
      | Some _ -> Maybe_bound.Incl k (* Key exists, use it directly *)
      | None ->
        (* Key doesn't exist, find nearest key *)
        (match find_gte_key m k with
         | Some gte_key -> Maybe_bound.Incl gte_key
         | None ->
           (match find_lte_key m k with
            | Some lte_key -> Maybe_bound.Excl lte_key
            | None -> Maybe_bound.Unbounded))
    in
    (* Transforms a key into a [Maybe_bound.t]. If the key isn't in the dataset, attempt
       to use the nearest key in the dataset. *)
    let lookup_upper_bound (k : (k, w) Incremental.t) =
      let%mapn m and k in
      match Map.find m k with
      | Some _ -> Maybe_bound.Incl k (* Key exists, use it directly *)
      | None ->
        (* Key doesn't exist, find nearest key *)
        (match find_lte_key m k with
         | Some lte_key -> Maybe_bound.Incl lte_key
         | None ->
           (match find_gte_key m k with
            | Some gte_key -> Maybe_bound.Excl gte_key
            | None -> Maybe_bound.Unbounded))
    in
    let key_range =
      to_bounds_pair ~lookup_lower_bound ~lookup_upper_bound which_key_range
    in
    Rank_from_start.Range.of_key_range ~key_to_rank_instrumentation ~data:m key_range
  | Sorted (data, _) ->
    let lookup_lower_bound (k : (k, w) Incremental.t)
      : ((k * v) Maybe_bound.t, w) Incremental.t
      =
      let%mapn orig_map and k in
      match Map.find orig_map k with
      | Some v -> Maybe_bound.Incl (k, v)
      | None ->
        (* We can't guess a likely position for the key as the dataset is sorted by value *)
        Maybe_bound.Unbounded
    in
    let lookup_upper_bound (k : (k, w) Incremental.t)
      : ((k * v) Maybe_bound.t, w) Incremental.t
      =
      let%mapn orig_map and k in
      match Map.find orig_map k with
      | Some v -> Maybe_bound.Incl (k, v)
      | None ->
        (* We can't guess a likely position for the key as the dataset is sorted by value *)
        Maybe_bound.Unbounded
      (* Target larger than all keys *)
    in
    let key_range =
      to_bounds_pair ~lookup_lower_bound ~lookup_upper_bound which_key_range
    in
    Rank_from_start.Range.of_key_range ~key_to_rank_instrumentation ~data key_range
;;

(** Apply the key range and rank range in order. Bounds in the rank range are relative to
    the data after the key range has been applied.

    Algorithmically, we take [which_key_range], narrow it based on [which_rank_range], and
    widen it based on [widen_range_by]. To simplify the logic, each range is normalized to
    a [Rank_from_start.Range.t] so that ranges can be easily combined into a single call
    to [Incr_map.subrange_by_rank]. *)
let do_range_restrict
  orig_data
  data
  ~which_key_range
  ~which_rank_range
  ~widen_range_by
  ~key_to_rank_instrumentation
  ~subrange_by_rank_instrumentation
  =
  let num_filtered_rows = Incr_collated_map.length data in
  let key_rank = Incr_collated_map.key_rank data in
  let data_length_for_key_range = Incr_collated_map.length data in
  let key_range_as_rank_range_from_start =
    which_key_range_to_rank_range_from_start
      ~key_to_rank_instrumentation
      data
      ~orig_map:orig_data
      ~which_key_range
  in
  let data_length_of_key_range =
    let%mapn data_length_for_key_range and key_range_as_rank_range_from_start in
    Rank_from_start.Range.length
      ~data_length:data_length_for_key_range
      key_range_as_rank_range_from_start
    |> (* Guard against reversed/empty key ranges producing negative lengths. For the
          purpose of interpreting rank_range (which is relative to the key-range), a
          reversed key-range should behave like an empty segment of length 0. *)
    Int.max 0
  in
  let combined_range_before_widening =
    let%mapn data_length = data_length_of_key_range
    and which_rank_range
    and key_range_as_rank_range_from_start in
    let rank_range =
      Rank_from_start.Range.of_which_rank_range ~data_length which_rank_range
    in
    Rank_from_start.Range.remove_basis
      ~basis:key_range_as_rank_range_from_start
      rank_range
  in
  let%pattern_bind combined_range_for_selecting, range_widened_by =
    let%mapn widen_range_by
    and data_length_for_key_range
    and combined_range_before_widening in
    Rank_from_start.Range.widen
      ~by:widen_range_by
      ~data_length:data_length_for_key_range
      combined_range_before_widening
  in
  (* [range_widened_by] is a tuple and therefore breaks phys_equal *)
  Incremental.set_cutoff
    range_widened_by
    (Incremental.Cutoff.of_equal [%equal: int * int]);
  let data =
    do_rank_range_restrict
      ~subrange_by_rank_instrumentation
      data
      ~rank_range:combined_range_for_selecting
  in
  let data = do_to_pos_map data in
  let collated =
    let%mapn data
    and num_unfiltered_rows = orig_data >>| Map.length
    and num_filtered_rows
    and which_key_range
    and which_rank_range
    and combined_range_before_widening
    and data_length_of_key_range
    and range_widened_by in
    let num_before_range =
      Rank_from_start.Range.count_before combined_range_before_widening
      |> Int.min num_filtered_rows
    in
    let which_rank_range =
      Which_range.to_from_start_rank_range
        which_rank_range
        ~data_length:data_length_of_key_range
    in
    Collated.Private.create
      ~data
      ~num_filtered_rows
      ~key_range:which_key_range
      ~rank_range:which_rank_range
      ~num_before_range
      ~range_widened_by
      ~num_unfiltered_rows
  in
  Incremental.both collated key_rank
;;

module Instrumentation = struct
  type t =
    { key_to_rank : Incr_map.Instrumentation.t
    ; subrange_by_rank : Incr_map.Instrumentation.t
    ; filter : Incr_map.Instrumentation.t
    ; fold : Incr_map.Instrumentation.t
    ; sort : Incr_map.Instrumentation.t
    }
end

let collate_and_maybe_fold
  (type k v cmp filter order w fold_result)
  ?(operation_order = `Sort_first)
  ~filter_equal
  ~order_equal
  ~(filter_to_predicate : filter -> _)
  ~(order_to_compare : order -> _)
  ~(fold_action : (k, v, fold_result) Fold_action.t)
  ~(instrumentation : Instrumentation.t option)
  (data : ((k, v, cmp) Map.t, w) Incremental.t)
  (collate : ((k, filter, order) Collate_params.t, w) Incremental.t)
  : (k, v, cmp, fold_result, w) t
  =
  let%pattern_bind.Option { key_to_rank = key_to_rank_instrumentation
                          ; subrange_by_rank = subrange_by_rank_instrumentation
                          ; filter = filter_instrumentation
                          ; fold = fold_instrumentation
                          ; sort = sort_instrumentation
                          }
    =
    instrumentation
  in
  let incremental_state = Incremental.state data in
  let%pattern_bind (collated, key_rank), fold_result =
    let%bind map_comparator = Incremental.freeze (data >>| Map.comparator) in
    let%pattern_bind { key_range; rank_range; filter; order; widen_range_by } = collate in
    let filter = with_cutoff filter ~equal:filter_equal in
    let order = with_cutoff order ~equal:order_equal in
    let orig_data = data in
    match operation_order with
    | `Filter_first ->
      let%bind filter in
      let predicate = filter_to_predicate filter in
      let data = do_filter ~instrumentation:filter_instrumentation data ~predicate in
      let fold_result =
        do_fold
          ~instrumentation:fold_instrumentation
          (Original data)
          ~incremental_state
          ~in_scope:(fun f -> f ())
          ~fold_action
      in
      let%bind order in
      let compare = order_to_compare order in
      let (T custom_comparator) = comparator_of_compare ~map_comparator compare in
      let data =
        do_sort
          ~instrumentation:sort_instrumentation
          data
          ~map_comparator
          ~custom_comparator
      in
      let%mapn out =
        do_range_restrict
          ~key_to_rank_instrumentation
          ~subrange_by_rank_instrumentation
          orig_data
          data
          ~which_key_range:key_range
          ~which_rank_range:rank_range
          ~widen_range_by
      and fold_result in
      out, fold_result
    | `Sort_first ->
      let%bind order in
      let compare = order_to_compare order in
      let (T custom_comparator) = comparator_of_compare ~map_comparator compare in
      let data =
        do_sort
          ~instrumentation:sort_instrumentation
          data
          ~map_comparator
          ~custom_comparator
      in
      let%bind filter in
      let predicate = filter_to_predicate filter in
      let data =
        do_filter_sorted ~instrumentation:filter_instrumentation data ~predicate
      in
      let fold_result =
        do_fold
          ~instrumentation:fold_instrumentation
          data
          ~incremental_state
          ~in_scope:(fun f -> f ())
          ~fold_action
      in
      let%mapn out =
        do_range_restrict
          ~key_to_rank_instrumentation
          ~subrange_by_rank_instrumentation
          orig_data
          data
          ~which_key_range:key_range
          ~which_rank_range:rank_range
          ~widen_range_by
      and fold_result in
      out, fold_result
  in
  { collated; key_rank; fold_result }
;;

let collate
  ?operation_order
  ?instrumentation
  ~filter_equal
  ~order_equal
  ~filter_to_predicate
  ~order_to_compare
  data
  c
  =
  collate_and_maybe_fold
    ?operation_order
    ~instrumentation
    ~filter_equal
    ~order_equal
    ~filter_to_predicate
    ~order_to_compare
    ~fold_action:Fold_action.Don't_fold
    data
    c
;;

let collate_and_fold
  ?operation_order
  ?instrumentation
  ~filter_equal
  ~order_equal
  ~filter_to_predicate
  ~order_to_compare
  ~fold
  data
  c
  =
  collate_and_maybe_fold
    ?operation_order
    ~instrumentation
    ~filter_equal
    ~order_equal
    ~filter_to_predicate
    ~order_to_compare
    ~fold_action:(Fold_action.Fold fold)
    data
    c
;;

module With_caching = struct
  module Range_memoize_bucket = Range_memoize_bucket

  let collate_and_maybe_fold__sort_first
    (type k v cmp filter order fold_result w)
    ~filter_equal
    ~order_equal
    ?(order_cache_params = Store_params.alist_based__lru ~equal:order_equal ~max_size:10)
    ?(order_filter_cache_params =
      Store_params.alist_based__lru
        ~equal:(Tuple2.equal ~eq1:order_equal ~eq2:filter_equal)
        ~max_size:30)
    ?(order_filter_range_cache_params =
      Store_params.alist_based__lru
        ~equal:
          (Tuple3.equal
             ~eq1:order_equal
             ~eq2:filter_equal
             ~eq3:Range_memoize_bucket.equal)
        ~max_size:50)
    ?(range_memoize_bucket_size = 10000)
    ~(instrumentation : Instrumentation.t option)
    ~(filter_to_predicate : filter -> _)
    ~(order_to_compare : order -> _)
    ~(fold_action : (k, v, fold_result) Fold_action.t)
    (data : ((k, v, cmp) Map.t, w) Incremental.t)
    (collate : ((k, filter, order) Collate_params.t, w) Incremental.t)
    : (k, v, cmp, fold_result, w) t
    =
    let%pattern_bind.Option { key_to_rank = key_to_rank_instrumentation
                            ; subrange_by_rank = subrange_by_rank_instrumentation
                            ; filter = filter_instrumentation
                            ; fold = fold_instrumentation
                            ; sort = sort_instrumentation
                            }
      =
      instrumentation
    in
    let%pattern_bind (collated, key_rank), fold_result =
      let cache_sorted = Store.create order_cache_params in
      let cache_sorted_filtered = Store.create order_filter_cache_params in
      let cache_sorted_filtered_ranked = Store.create order_filter_range_cache_params in
      let%bind map_comparator = Incremental.freeze (data >>| Map.comparator) in
      let%pattern_bind { key_range; rank_range; filter; order; widen_range_by } =
        collate
      in
      let incremental_state = Incremental.state key_range in
      let range_bucket =
        (*=Range operations are incremental with respect to the range, so we don't have
           to bind to ranges.

           However, incrementality does not necessarily mean they're fast - they run in
           roughly O(|new ranks - old ranks|) or O(|new key position - old key position|).
           So, if we request two very different ranges one after another, the computation
           will be very expensive.

           We alleviate this problem here by dividing possible ranges into buckets, and
           only using incremental computation when old & new belong to the same bucket.

           We also keep a cache of a few least recently used buckets.
        *)
        let%map key_range and rank_range in
        (* At this point, we cannot get [num_filtered_row] so we set it to 0 to get a
           forward rank range. This means that [Backward] Ranks in the forward rank_range
           will be negative. This is fine because this [rank_range] isn't passed to
           [Incr_map.subrange_by_rank] but only used to uniquely identify a Bucket range.
           For that use case, a negative range should be fine... *)
        let data_length = 0 in
        let rank_range = Which_range.to_from_start_rank_range ~data_length rank_range in
        Range_memoize_bucket.create
          ~bucket_size:range_memoize_bucket_size
          ~key_range
          ~rank_range
      in
      let orig_data = data in
      let scope = Incremental.Scope.current incremental_state () in
      let in_scope f = Incremental.Scope.within incremental_state scope ~f in
      let%bind order and filter and range_bucket in
      (* This line causes the computation below to always be executed. This is fine, as it
         consists only of cache lookups, which are cheap. And we want to execute them to
         get more accurate LRU caches & hooks behaviour. *)
      let never_cutoff = Incremental.return incremental_state () in
      Incremental.set_cutoff never_cutoff Incremental.Cutoff.never;
      let%bind () = never_cutoff in
      let compare = order_to_compare order in
      let predicate = filter_to_predicate filter in
      let do_range ~sorted ~sorted_filtered ~fold_result =
        let sorted_filtered_ranked =
          in_scope (fun () ->
            do_range_restrict
              ~key_to_rank_instrumentation
              ~subrange_by_rank_instrumentation
              orig_data
              sorted_filtered
              ~which_key_range:key_range
              ~which_rank_range:rank_range
              ~widen_range_by)
        in
        Store.add
          cache_sorted_filtered_ranked
          ~key:(order, filter, range_bucket)
          ~value:(sorted, sorted_filtered, fold_result, sorted_filtered_ranked);
        Incremental.both sorted_filtered_ranked fold_result
      in
      let do_filter_range ~sorted =
        let sorted_filtered =
          in_scope (fun () ->
            do_filter_sorted ~instrumentation:filter_instrumentation sorted ~predicate)
        in
        let fold_result =
          do_fold
            ~instrumentation:fold_instrumentation
            sorted_filtered
            ~incremental_state
            ~in_scope
            ~fold_action
        in
        Store.add
          cache_sorted_filtered
          ~key:(order, filter)
          ~value:(sorted, sorted_filtered, fold_result);
        do_range ~sorted ~sorted_filtered ~fold_result
      in
      let do_sort_filter_range () =
        let (T custom_comparator) = comparator_of_compare ~map_comparator compare in
        let sorted =
          in_scope (fun () ->
            do_sort
              ~instrumentation:sort_instrumentation
              data
              ~map_comparator
              ~custom_comparator)
        in
        Store.add cache_sorted ~key:order ~value:sorted;
        do_filter_range ~sorted
      in
      (* We implement "lazy eviction" here - we only allow ourselves to use a value from
         deeper cache if its partial computations are present in the earlier layers.

         E.g. if we evict some ordering "s" from [cache_sorted], we might still have "s,
         f, r" in [cache_sorted_filtered_ranked], but we won't use it, and instead
         recreate from scratch and overwrite the cache.

         This guarantees that, in presence of evicting from earlier layers, we won't
         duplicate computations.
      *)
      let sorted = Store.find cache_sorted order in
      let sorted_filtered = Store.find cache_sorted_filtered (order, filter) in
      let sorted_filtered_ranked =
        Store.find cache_sorted_filtered_ranked (order, filter, range_bucket)
      in
      match sorted, sorted_filtered, sorted_filtered_ranked with
      | Some s, Some (s', sf, fr), Some (s'', sf', fr', sfr)
        when phys_equal s s' && phys_equal s s'' && phys_equal sf sf' && phys_equal fr fr'
        -> Incremental.both sfr fr
      | Some s, Some (s', sf, fr), _ when phys_equal s s' ->
        do_range ~sorted:s ~sorted_filtered:sf ~fold_result:fr
      | Some sorted, _, _ -> do_filter_range ~sorted
      | None, _, _ -> do_sort_filter_range ()
    in
    { collated; key_rank; fold_result }
  ;;

  let collate__sort_first
    (type k v cmp filter order w)
    ~filter_equal
    ~order_equal
    ?order_cache_params
    ?order_filter_cache_params
    ?order_filter_range_cache_params
    ?range_memoize_bucket_size
    ?instrumentation
    ~(filter_to_predicate : filter -> _)
    ~(order_to_compare : order -> _)
    (data : ((k, v, cmp) Map.t, w) Incremental.t)
    (collate : ((k, filter, order) Collate_params.t, w) Incremental.t)
    : (k, v, cmp, unit, w) t
    =
    collate_and_maybe_fold__sort_first
      ~filter_equal
      ~order_equal
      ?order_cache_params
      ?order_filter_cache_params
      ?order_filter_range_cache_params
      ?range_memoize_bucket_size
      ~instrumentation
      ~filter_to_predicate
      ~order_to_compare
      ~fold_action:Don't_fold
      data
      collate
  ;;

  let collate_and_fold__sort_first
    (type k v cmp filter order fold_result w)
    ~filter_equal
    ~order_equal
    ?order_cache_params
    ?order_filter_cache_params
    ?order_filter_range_cache_params
    ?range_memoize_bucket_size
    ?instrumentation
    ~(filter_to_predicate : filter -> _)
    ~(order_to_compare : order -> _)
    ~(fold : (k, v, fold_result) Fold_params.t)
    (data : ((k, v, cmp) Map.t, w) Incremental.t)
    (collate : ((k, filter, order) Collate_params.t, w) Incremental.t)
    : (k, v, cmp, fold_result, w) t
    =
    collate_and_maybe_fold__sort_first
      ~filter_equal
      ~order_equal
      ?order_cache_params
      ?order_filter_cache_params
      ?order_filter_range_cache_params
      ?range_memoize_bucket_size
      ~instrumentation
      ~filter_to_predicate
      ~order_to_compare
      ~fold_action:(Fold fold)
      data
      collate
  ;;
end
