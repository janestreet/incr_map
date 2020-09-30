open! Core_kernel
module Collate = Collate
module Collated = Collated
module Map_list = Map_list

module Compare = struct
  type ('k, 'v) t =
    | Unchanged
    | Reversed
    | Custom_by_value of { compare : 'v -> 'v -> int }
    | Custom_by_key_and_value of { compare : 'k * 'v -> 'k * 'v -> int }
  [@@deriving sexp_of]
end

module Make (Incr : Incremental.S) = struct
  module Incr_map = Incr_map.Make (Incr)
  module Incr_memoize = Incr_memoize.Make (Incr)
  module Collate = Collate
  module Collated = Collated
  module Compare = Compare

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

  module Incr_collated_map = struct
    type ('k, 'v, 'cmp, 'custom_cmp) t =
      | Original of (('k, 'v, 'cmp) Map.t Incr.t * ('k, 'cmp) Comparator.t)
      | Sorted of
          (* This type is tricky. We store the value *twice*, both in
             the map's key and in map's data.

             ! you should always use the value from the data !

             We need it in map's key, to be able to sort by data from values. However, the
             (key, value) comparator does not provide a linear order on all possible
             tuples. It is the case for all the tuples in the map at any given time
             (as keys are unique, and we incorporate key comparator in
             [Custom_tuple_comparator]), but not when diffing two maps!

             For a concrete example, if we have [key = int] and
             [value = (Symbol.t * Price.t)], sort the map by symbols, and then update
             price (e.g. change from (1, (AAPL, $1)) to (1, (AAPL, $2))), the
             [Custom_tuple_comparator] will (rightfully) say the two are equal -
             this entry doesn't have to move in the sorted map.

             When diffing maps (so in ~every Incr_map function), on such
             a change we will only notice that the data changed (i.e. symmetric_diff will
             return `Unequal), and not that the key changed. Therefore, most of the
             functions (e.g. filter, subrange) will only replace the value for the key,
             and not update the key.

             ! this means the two values in a single entry might not be equal [0] !

             This is fine though. It suffices to just use the value from the data
             everywhere where we care about something more than the ordering.
             It will stay up-to-date, as all incr_map operations (obviously!) need to
             support changing a value without key changing. In places where we only care
             about ordering, using either is fine - as they compare equal.

             So, for filtering and the final conversion to a list we properly use the
             value from the data. The other operations - sorting, subrange,
             subrange_by_rank, only care about the order, so they're fine [1]

             [0] Note that to restore consistency (i.e. to always have value in key =
             value in data) we would have to handle this in *all* incremental operations
             that we do after sorting - so we would need to copy a decent part of Incr_map.

             [1] there is one more catch with subrange - we pass it (key, new_value) as
             key, as we do the lookup in the original map, but it might only have (key,
             old_value) in the map it sees. It's still fine, as the two tuples compare
             equal.
          *)
          (('k * 'v, 'v, 'custom_cmp) Map.t Incr.t * ('k * 'v, 'custom_cmp) Comparator.t)

    let length t =
      let open Incr.Let_syntax in
      match t with
      | Original (m, _) -> m >>| Map.length
      | Sorted (m, _) -> m >>| Map.length
    ;;
  end

  open Incr.Let_syntax

  let do_filter data ~predicate =
    match predicate with
    | None -> data
    | Some filter ->
      Incr_map.filter_mapi data ~f:(fun ~key ~data ->
        if filter ~key ~data then Some data else None)
  ;;

  let do_filter_sorted (data : _ Incr_collated_map.t) ~predicate : _ Incr_collated_map.t =
    let filter (type a c) ~(get : a -> 'v -> 'k * 'v) (m : (a, 'v, c) Map.t Incr.t)
      : (a, 'v, c) Map.t Incr.t
      =
      match predicate with
      | None -> m
      | Some filter ->
        Incr_map.filter_mapi m ~f:(fun ~key ~data ->
          let key, data = get key data in
          if filter ~key ~data then Some data else None)
    in
    match data with
    | Original (m, cmp) -> Original (filter ~get:(fun k v -> k, v) m, cmp)
    | Sorted (m, cmp) -> Sorted (filter ~get:(fun (k, _v1) v2 -> k, v2) m, cmp)
  ;;

  let do_sort
        (type k v cmp custom_cmp)
        (data : (k, v, cmp) Map.t Incr.t)
        ~(map_comparator : (k, cmp) Comparator.t)
        ~(custom_comparator : (k * v, custom_cmp) Comparator.t option)
    : (k, v, cmp, custom_cmp) Incr_collated_map.t
    =
    match custom_comparator with
    | None -> Incr_collated_map.Original (data, map_comparator)
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
      Sorted (sorted, custom_comparator)
  ;;

  let do_rank_range_restrict_and_rank
        (type k v cmp custom_cmp)
        (data : (k, v, cmp, custom_cmp) Incr_collated_map.t)
        ~(rank_range : int Collate.Which_range.t Incr.t)
    : (k, v, cmp, custom_cmp) Incr_collated_map.t * int Incr.t
    =
    let apply_range data =
      match%pattern_bind rank_range with
      | All_rows -> data
      | Between (l, u) ->
        Incr_map.subrange_by_rank data (Incr.map2 l u ~f:(fun l u -> Incl l, Incl u))
      | From l -> Incr_map.subrange_by_rank data (l >>| fun l -> Incl l, Unbounded)
      | To u -> Incr_map.subrange_by_rank data (u >>| fun u -> Unbounded, Incl u)
    in
    let count_before =
      match%pattern_bind rank_range with
      | All_rows | To _ -> Incr.return 0
      | Between (l, _) | From l -> l
    in
    match data with
    | Original (m, cmp) -> Original (apply_range m, cmp), count_before
    | Sorted (m, cmp) -> Sorted (apply_range m, cmp), count_before
  ;;

  let do_key_range_restrict
        (type k v cmp custom_cmp)
        (data : (k, v, cmp, custom_cmp) Incr_collated_map.t)
        ~(orig_map : (k, v, cmp) Map.t Incr.t)
        ~(key_range : k Collate.Which_range.t Incr.t)
    : (k, v, cmp, custom_cmp) Incr_collated_map.t * int Incr.t
    =
    let resolve_range_and_do
          (type full_key)
          (data : (full_key, _, _) Map.t Incr.t)
          ~(lookup : k Incr.t -> full_key Maybe_bound.t Incr.t)
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
      | Original (map, _cmp) ->
        (match%pattern_bind key_range with
         | All_rows | To _ -> Incr.return None
         | Between (k, _) | From k ->
           let closest =
             let%map key = k
             and map = map in
             Map.closest_key map `Less_or_equal_to key
           in
           (match%pattern_bind closest with
            | Some (k, _) -> Incr_map.rank map k
            | None -> Incr.return None))
      | Sorted (map, _cmp) ->
        (match%pattern_bind key_range with
         | All_rows | To _ -> Incr.return None
         | Between (k, _) | From k ->
           let v =
             let%map orig_map = orig_map
             and k = k in
             Map.find orig_map k
           in
           (match%pattern_bind v with
            | None -> Incr.return None
            | Some v ->
              let closest =
                let%map key = k
                and v = v
                and map = map in
                Map.closest_key map `Less_or_equal_to (key, v)
              in
              (match%pattern_bind closest with
               | Some (k, _) -> Incr_map.rank map k
               | None -> Incr.return None)))
    in
    let count_before = Incr.map count_before ~f:(Option.value ~default:0) in
    match data with
    | Original (data, cmp) ->
      let lookup k =
        let%map k = k in
        Maybe_bound.Incl k
      in
      Original (resolve_range_and_do data ~lookup, cmp), count_before
    | Sorted (data, cmp) ->
      let lookup k =
        let%map orig_map = orig_map
        and k = k in
        match Map.find orig_map k with
        | None -> Maybe_bound.Unbounded
        | Some v -> Maybe_bound.Incl (k, v)
      in
      Sorted (resolve_range_and_do data ~lookup, cmp), count_before
  ;;

  type ('k, 'v) kv_custom_comparator =
    | T : ('k * 'v, _) Comparator.t option -> ('k, 'v) kv_custom_comparator

  let comparator_of_compare
        (type k v)
        ~(map_comparator : (k, _) Comparator.t)
        (compare : (k, v) Compare.t)
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
    Incr.set_cutoff incr (Incremental.Cutoff.of_equal equal);
    incr
  ;;

  let do_to_pos_map
        (type k v cmp custom_cmp)
        (data : (k, v, cmp, custom_cmp) Incr_collated_map.t)
    =
    match data with
    | Original (data, _cmp) -> Map_list.of_map data ~get:(fun ~key ~data -> key, data)
    | Sorted (data, _cmp) ->
      Map_list.of_map data ~get:(fun ~key:(k, _v1) ~data:v2 -> k, v2)
  ;;

  (* Incrementally compute all of our transformations. *)
  let collate
        (type k v cmp filter order)
        ?(operation_order = `Sort_first)
        ?filter_memoize_params
        ?order_memoize_params
        ~filter_equal
        ~order_equal
        ~(filter_to_predicate : filter -> _)
        ~(order_to_compare : order -> _)
        (data : (k, v, cmp) Map.t Incr.t)
        (collate : (k, filter, order) Collate.t Incr.t)
    : (k, v) Collated.t Incr.t
    =
    let%bind map_comparator = data >>| Map.comparator in
    let%pattern_bind { key_range; rank_range; filter; order } = collate in
    let filter = with_cutoff filter ~equal:filter_equal in
    let order = with_cutoff order ~equal:order_equal in
    let filter =
      Incr_memoize.with_params
        filter
        (Option.value
           filter_memoize_params
           ~default:
             (Incr_memoize.Store_params.alist_based__lru ~equal:filter_equal ~max_size:1))
    in
    let order =
      Incr_memoize.with_params
        order
        (Option.value
           order_memoize_params
           ~default:
             (Incr_memoize.Store_params.alist_based__lru ~equal:order_equal ~max_size:10))
    in
    let orig_data = data in
    let with_filtered_and_sorted data ~num_filtered_rows =
      let data, count_before_key_rank =
        do_key_range_restrict data ~key_range ~orig_map:orig_data
      in
      let data, count_before_range_rank =
        do_rank_range_restrict_and_rank data ~rank_range
      in
      let data = do_to_pos_map data in
      let%map data = data
      and num_filtered_rows = num_filtered_rows
      and num_unfiltered_rows = orig_data >>| Map.length
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
    match operation_order with
    | `Filter_first ->
      let%bind.Incr_memoize filter = filter in
      let predicate = filter_to_predicate filter in
      let data = do_filter data ~predicate in
      let num_filtered_rows = data >>| Map.length in
      let%bind.Incr_memoize order = order in
      let compare = order_to_compare order in
      let (T custom_comparator) = comparator_of_compare ~map_comparator compare in
      let data = do_sort data ~map_comparator ~custom_comparator in
      with_filtered_and_sorted data ~num_filtered_rows
    | `Sort_first ->
      let%bind.Incr_memoize order = order in
      let compare = order_to_compare order in
      let (T custom_comparator) = comparator_of_compare ~map_comparator compare in
      let data = do_sort data ~map_comparator ~custom_comparator in
      let%bind.Incr_memoize filter = filter in
      let predicate = filter_to_predicate filter in
      let data = do_filter_sorted data ~predicate in
      let num_filtered_rows = Incr_collated_map.length data in
      with_filtered_and_sorted data ~num_filtered_rows
  ;;
end
