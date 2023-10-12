open! Core
module Collate = Collate
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
  include Comparator.Derived2 (struct
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
  include Comparable.Make_plain (T)

  let create
    ~bucket_size
    ~(key_range : _ Collate.Which_range.t)
    ~(rank_range : int Collate.Which_range.t)
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
  type ('k, 'v, 'w) t =
    | Original : (('k, 'v, 'cmp) Map.t, 'w) Incremental.t -> ('k, 'v, 'w) t
    | Sorted :
        ((('k * 'v, 'v, 'custom_cmp) Map.t, 'w) Incremental.t * ('k, 'cmp) Comparator.t)
        -> ('k, 'v, 'w) t

  let length t =
    let open Incremental.Let_syntax in
    match t with
    | Original m -> m >>| Map.length
    | Sorted (m, _) -> m >>| Map.length
  ;;

  let key_rank t =
    let open Incremental.Let_syntax in
    match t with
    | Original m ->
      let%map m = m in
      fun key -> Map.rank m key
    | Sorted (m, key_comparator) ->
      let%map m = m in
      fun key ->
        let compare = key_comparator.compare in
        Map.to_sequence m
        |> Sequence.findi ~f:(fun _i ((k, _), _) -> compare k key = 0)
        |> Option.map ~f:fst
  ;;
end

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

let do_filter data ~predicate =
  match predicate with
  | None -> data
  | Some filter ->
    Incr_map.filter_mapi data ~f:(fun ~key ~data ->
      if filter ~key ~data then Some data else None)
;;

let do_filter_sorted (data : _ Incr_collated_map.t) ~predicate : _ Incr_collated_map.t =
  let filter
    (type a c)
    ~(get : a -> 'v -> 'k * 'v)
    (m : ((a, 'v, c) Map.t, 'w) Incremental.t)
    : ((a, 'v, c) Map.t, 'w) Incremental.t
    =
    match predicate with
    | None -> m
    | Some filter ->
      Incr_map.filter_mapi m ~f:(fun ~key ~data ->
        let key, data = get key data in
        if filter ~key ~data then Some data else None)
  in
  match data with
  | Original m -> Original (filter ~get:(fun k v -> k, v) m)
  | Sorted (m, key_cmp) -> Sorted (filter ~get:(fun (k, _v1) v2 -> k, v2) m, key_cmp)
;;

let do_fold
  (data : _ Incr_collated_map.t)
  ({ init; add; remove; update; finalize; revert_to_init_when_empty } : _ Fold_params.t)
  =
  match data with
  | Original map -> Incr_map.unordered_fold map ~init ~add ~remove ?update
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
  ~incremental_state
  ~in_scope
  ~(fold_action : (_, _, fold_result) Fold_action.t)
  =
  match fold_action with
  | Fold_action.Fold fold_params -> in_scope (fun () -> do_fold data fold_params)
  | Don't_fold -> in_scope (fun () -> Incremental.return incremental_state ())
;;

let do_sort
  (type k v cmp custom_cmp w)
  (data : ((k, v, cmp) Map.t, w) Incremental.t)
  ~(map_comparator : (k, cmp) Comparator.t)
  ~(custom_comparator : (k * v, custom_cmp) Comparator.t option)
  : (k, v, w) Incr_collated_map.t
  =
  match custom_comparator with
  | None -> Incr_collated_map.Original data
  | Some custom_comparator ->
    let sorted =
      Incr_map.unordered_fold
        ~init:(Map.Using_comparator.empty ~comparator:custom_comparator)
        ~add:(fun ~key ~data map -> Map.set map ~key:(key, data) ~data)
        ~remove:(fun ~key ~data map -> Map.remove map (key, data))
        ~specialized_initial:(fun ~init new_in ->
          let[@inline always] compare_ignoring_second (t1, _) (t2, _) =
            custom_comparator.compare t1 t2
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

let do_rank_range_restrict_and_rank
  (type k v w)
  (data : (k, v, w) Incr_collated_map.t)
  ~(rank_range : (int Collate.Which_range.t, w) Incremental.t)
  : (k, v, w) Incr_collated_map.t * (int, w) Incremental.t
  =
  let incremental_state = Incremental.state rank_range in
  let apply_range data =
    match%pattern_bind rank_range with
    | All_rows -> data
    | Between (l, u) ->
      Incr_map.subrange_by_rank data (Incremental.map2 l u ~f:(fun l u -> Incl l, Incl u))
    | From l -> Incr_map.subrange_by_rank data (l >>| fun l -> Incl l, Unbounded)
    | To u -> Incr_map.subrange_by_rank data (u >>| fun u -> Unbounded, Incl u)
  in
  let count_before =
    match%pattern_bind rank_range with
    | All_rows | To _ -> Incremental.return incremental_state 0
    | Between (l, _) | From l -> l
  in
  match data with
  | Original m -> Original (apply_range m), count_before
  | Sorted (m, key_cmp) -> Sorted (apply_range m, key_cmp), count_before
;;

let do_key_range_restrict
  (type k v cmp w)
  (data : (k, v, w) Incr_collated_map.t)
  ~(orig_map : ((k, v, cmp) Map.t, w) Incremental.t)
  ~(key_range : (k Collate.Which_range.t, w) Incremental.t)
  : (k, v, w) Incr_collated_map.t * (int, w) Incremental.t
  =
  let incremental_state = Incremental.state orig_map in
  let none = Incremental.return incremental_state None in
  let resolve_range_and_do
    (type full_key)
    (data : ((full_key, _, _) Map.t, w) Incremental.t)
    ~(lookup : (k, w) Incremental.t -> (full_key Maybe_bound.t, w) Incremental.t)
    =
    match%pattern_bind key_range with
    | All_rows -> data
    | Between (l, u) ->
      let range =
        let%map l = lookup l
        and u = lookup u in
        Some (l, u)
      in
      Incr_map.subrange data range
    | From l ->
      let range =
        let%map l = lookup l in
        Some (l, Maybe_bound.Unbounded)
      in
      Incr_map.subrange data range
    | To u ->
      let range =
        let%map u = lookup u in
        Some (Maybe_bound.Unbounded, u)
      in
      Incr_map.subrange data range
  in
  let count_before =
    match data with
    | Original map ->
      (match%pattern_bind key_range with
       | All_rows | To _ -> none
       | Between (k, _) | From k ->
         let closest =
           let%map key = k
           and map = map in
           Map.closest_key map `Less_or_equal_to key
         in
         (match%pattern_bind closest with
          | Some (k, _) -> Incr_map.rank map k
          | None -> none))
    | Sorted (map, _key_cmp) ->
      (match%pattern_bind key_range with
       | All_rows | To _ -> none
       | Between (k, _) | From k ->
         let v =
           let%map orig_map = orig_map
           and k = k in
           Map.find orig_map k
         in
         (match%pattern_bind v with
          | None -> none
          | Some v ->
            let closest =
              let%map key = k
              and v = v
              and map = map in
              Map.closest_key map `Less_or_equal_to (key, v)
            in
            (match%pattern_bind closest with
             | Some (k, _) -> Incr_map.rank map k
             | None -> none)))
  in
  let count_before = Incremental.map count_before ~f:(Option.value ~default:0) in
  match data with
  | Original data ->
    let lookup k =
      let%map k = k in
      Maybe_bound.Incl k
    in
    Original (resolve_range_and_do data ~lookup), count_before
  | Sorted (data, key_cmp) ->
    let lookup k =
      let%map orig_map = orig_map
      and k = k in
      match Map.find orig_map k with
      | None -> Maybe_bound.Unbounded
      | Some v -> Maybe_bound.Incl (k, v)
    in
    Sorted (resolve_range_and_do data ~lookup, key_cmp), count_before
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
          map_comparator.compare k2 k1)
        ~sexp_of_t:(fun (k, _v) -> map_comparator.sexp_of_t k)
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
          if res <> 0 then res else map_comparator.compare k1 k2)
        ~sexp_of_t:(fun (k, _v) -> map_comparator.sexp_of_t k)
    in
    T (Some Cmp.comparator)
;;

let with_cutoff incr ~equal =
  Incremental.set_cutoff incr (Incremental.Cutoff.of_equal equal);
  incr
;;

let do_to_pos_map (type k v w) (data : (k, v, w) Incr_collated_map.t) =
  match data with
  | Original data ->
    Opaque_map.erase_key_incrementally data ~get:(fun ~key ~data -> key, data)
  | Sorted (data, _key_cmp) ->
    Opaque_map.erase_key_incrementally data ~get:(fun ~key:(k, _v1) ~data:v2 -> k, v2)
;;

type ('k, 'v, 'fold_result, 'w) t =
  { collated : (('k, 'v) Collated.t, 'w) Incremental.t
  ; key_rank : ('k -> int option, 'w) Incremental.t
  ; fold_result : ('fold_result, 'w) Incremental.t
  }

let collated t = t.collated
let key_rank t = t.key_rank
let fold_result t = t.fold_result

let do_range_restrict orig_data data ~key_range ~rank_range =
  let num_filtered_rows = Incr_collated_map.length data in
  let key_rank = Incr_collated_map.key_rank data in
  let data, count_before_key_rank =
    do_key_range_restrict data ~key_range ~orig_map:orig_data
  in
  let data, count_before_range_rank = do_rank_range_restrict_and_rank data ~rank_range in
  let data = do_to_pos_map data in
  let collated =
    let%map data = data
    and num_unfiltered_rows = orig_data >>| Map.length
    and num_filtered_rows = num_filtered_rows
    and key_range = key_range
    and rank_range = rank_range
    and count_before_key_rank = count_before_key_rank
    and count_before_range_rank = count_before_range_rank in
    let num_before_range = count_before_key_rank + count_before_range_rank in
    Collated.Private.create
      ~data
      ~num_filtered_rows
      ~key_range
      ~rank_range
      ~num_before_range
      ~num_unfiltered_rows
  in
  Incremental.both collated key_rank
;;

let collate_and_maybe_fold
  (type k v cmp filter order w fold_result)
  ?(operation_order = `Sort_first)
  ~filter_equal
  ~order_equal
  ~(filter_to_predicate : filter -> _)
  ~(order_to_compare : order -> _)
  ~(fold_action : (k, v, fold_result) Fold_action.t)
  (data : ((k, v, cmp) Map.t, w) Incremental.t)
  (collate : ((k, filter, order) Collate.t, w) Incremental.t)
  : (k, v, fold_result, w) t
  =
  let incremental_state = Incremental.state data in
  let%pattern_bind (collated, key_rank), fold_result =
    let%bind map_comparator = Incremental.freeze (data >>| Map.comparator) in
    let%pattern_bind { key_range; rank_range; filter; order } = collate in
    let filter = with_cutoff filter ~equal:filter_equal in
    let order = with_cutoff order ~equal:order_equal in
    let orig_data = data in
    match operation_order with
    | `Filter_first ->
      let%bind filter = filter in
      let predicate = filter_to_predicate filter in
      let data = do_filter data ~predicate in
      let fold_result =
        do_fold (Original data) ~incremental_state ~in_scope:(fun f -> f ()) ~fold_action
      in
      let%bind order = order in
      let compare = order_to_compare order in
      let (T custom_comparator) = comparator_of_compare ~map_comparator compare in
      let data = do_sort data ~map_comparator ~custom_comparator in
      let%mapn out = do_range_restrict orig_data data ~key_range ~rank_range
      and fold_result = fold_result in
      out, fold_result
    | `Sort_first ->
      let%bind order = order in
      let compare = order_to_compare order in
      let (T custom_comparator) = comparator_of_compare ~map_comparator compare in
      let data = do_sort data ~map_comparator ~custom_comparator in
      let%bind filter = filter in
      let predicate = filter_to_predicate filter in
      let data = do_filter_sorted data ~predicate in
      let fold_result =
        do_fold data ~incremental_state ~in_scope:(fun f -> f ()) ~fold_action
      in
      let%mapn out = do_range_restrict orig_data data ~key_range ~rank_range
      and fold_result = fold_result in
      out, fold_result
  in
  { collated; key_rank; fold_result }
;;

let collate
  ?operation_order
  ~filter_equal
  ~order_equal
  ~filter_to_predicate
  ~order_to_compare
  data
  c
  =
  collate_and_maybe_fold
    ?operation_order
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
    ~(filter_to_predicate : filter -> _)
    ~(order_to_compare : order -> _)
    ~(fold_action : (k, v, fold_result) Fold_action.t)
    (data : ((k, v, cmp) Map.t, w) Incremental.t)
    (collate : ((k, filter, order) Collate.t, w) Incremental.t)
    : (k, v, fold_result, w) t
    =
    let%pattern_bind (collated, key_rank), fold_result =
      let cache_sorted = Store.create order_cache_params in
      let cache_sorted_filtered = Store.create order_filter_cache_params in
      let cache_sorted_filtered_ranked = Store.create order_filter_range_cache_params in
      let%bind map_comparator = Incremental.freeze (data >>| Map.comparator) in
      let%pattern_bind { key_range; rank_range; filter; order } = collate in
      let incremental_state = Incremental.state key_range in
      let range_bucket =
        (* Range operations are incremental with respect to the range, so we don't have
           to bind to ranges.

           However, incrementality does not necessarily mean they're fast - they run in
           roughly O(|new ranks - old ranks|) or O(|new key position - old key position|).
           So, if we request two very different ranges one after another, the computation
           will be very expensive.

           We alleviate this problem here by dividing possible ranges into buckets, and
           only using incremental computation when old & new belong to the same bucket.

           We also keep a cache of a few least recently used buckets.
        *)
        let%map key_range = key_range
        and rank_range = rank_range in
        Range_memoize_bucket.create
          ~bucket_size:range_memoize_bucket_size
          ~key_range
          ~rank_range
      in
      let orig_data = data in
      let scope = Incremental.Scope.current incremental_state () in
      let in_scope f = Incremental.Scope.within incremental_state scope ~f in
      let%bind order = order
      and filter = filter
      and range_bucket = range_bucket in
      (* This line causes the computation below to always be executed. This is fine,
         as it consists only of cache lookups, which are cheap. And we want to execute
         them to get more accurate LRU caches & hooks behaviour. *)
      let never_cutoff = Incremental.return incremental_state () in
      Incremental.set_cutoff never_cutoff Incremental.Cutoff.never;
      let%bind () = never_cutoff in
      let compare = order_to_compare order in
      let predicate = filter_to_predicate filter in
      let do_range ~sorted ~sorted_filtered ~fold_result =
        let sorted_filtered_ranked =
          in_scope (fun () ->
            do_range_restrict orig_data sorted_filtered ~key_range ~rank_range)
        in
        Store.add
          cache_sorted_filtered_ranked
          ~key:(order, filter, range_bucket)
          ~value:(sorted, sorted_filtered, fold_result, sorted_filtered_ranked);
        Incremental.both sorted_filtered_ranked fold_result
      in
      let do_filter_range ~sorted =
        let sorted_filtered = in_scope (fun () -> do_filter_sorted sorted ~predicate) in
        let fold_result =
          do_fold sorted_filtered ~incremental_state ~in_scope ~fold_action
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
          in_scope (fun () -> do_sort data ~map_comparator ~custom_comparator)
        in
        Store.add cache_sorted ~key:order ~value:sorted;
        do_filter_range ~sorted
      in
      (* We implement "lazy eviction" here - we only allow ourselves to use a value from
         deeper cache if its partial computations are present in the earlier layers.

         E.g. if we evict some ordering "s" from [cache_sorted], we might still have
         "s, f, r" in [cache_sorted_filtered_ranked], but we won't use it, and instead
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
    ~(filter_to_predicate : filter -> _)
    ~(order_to_compare : order -> _)
    (data : ((k, v, cmp) Map.t, w) Incremental.t)
    (collate : ((k, filter, order) Collate.t, w) Incremental.t)
    : (k, v, unit, w) t
    =
    collate_and_maybe_fold__sort_first
      ~filter_equal
      ~order_equal
      ?order_cache_params
      ?order_filter_cache_params
      ?order_filter_range_cache_params
      ?range_memoize_bucket_size
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
    ~(filter_to_predicate : filter -> _)
    ~(order_to_compare : order -> _)
    ~(fold : (k, v, fold_result) Fold_params.t)
    (data : ((k, v, cmp) Map.t, w) Incremental.t)
    (collate : ((k, filter, order) Collate.t, w) Incremental.t)
    : (k, v, fold_result, w) t
    =
    collate_and_maybe_fold__sort_first
      ~filter_equal
      ~order_equal
      ?order_cache_params
      ?order_filter_cache_params
      ?order_filter_range_cache_params
      ?range_memoize_bucket_size
      ~filter_to_predicate
      ~order_to_compare
      ~fold_action:(Fold fold)
      data
      collate
  ;;
end
