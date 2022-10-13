open! Core
module Collate = Collate
module Collated = Collated
module Map_list = Incr_map_erase_key
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

module Make (Incr : Incremental.S) = struct
  module Incr_map = Incr_map.Make (Incr)
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
    type ('k, 'v, 'cmp, 'custom_cmp) t =
      | Original of (('k, 'v, 'cmp) Map.t Incr.t * ('k, 'cmp) Comparator.t)
      | Sorted of
          (('k * 'v, 'v, 'custom_cmp) Map.t Incr.t * ('k * 'v, 'custom_cmp) Comparator.t)

    let length t =
      let open Incr.Let_syntax in
      match t with
      | Original (m, _) -> m >>| Map.length
      | Sorted (m, _) -> m >>| Map.length
    ;;

    type ('k, 'v, 'cmp) packed =
      | T : ('k, 'v, 'cmp, 'custom_cmp) t -> ('k, 'v, 'cmp) packed
    [@@unboxed]
  end

  module Fold_params = struct
    type ('k, 'v, 'acc) t =
      { init : 'acc
      ; add : key:'k -> data:'v -> 'acc -> 'acc
      ; remove : key:'k -> data:'v -> 'acc -> 'acc
      ; update : (key:'k -> old_data:'v -> new_data:'v -> 'acc -> 'acc) option
      ; revert_to_init_when_empty : bool
      }
  end

  module Fold_action = struct
    type ('k, 'v, 'acc) t =
      | Fold : ('k, 'v, 'acc) Fold_params.t -> ('k, 'v, 'acc) t
      | Don't_fold : ('k, 'v, unit) t
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

  let do_fold
        (data : _ Incr_collated_map.t)
        ({ init; add; remove; update; revert_to_init_when_empty } : _ Fold_params.t)
    =
    match data with
    | Original (map, _) -> Incr_map.unordered_fold map ~init ~add ~remove ?update
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
    Incr.set_cutoff incr (Incremental.Cutoff.of_equal equal);
    incr
  ;;

  let do_to_pos_map
        (type k v cmp custom_cmp)
        (data : (k, v, cmp, custom_cmp) Incr_collated_map.t)
    =
    match data with
    | Original (data, _cmp) -> Map_list.erase data ~get:(fun ~key ~data -> key, data)
    | Sorted (data, _cmp) ->
      Map_list.erase data ~get:(fun ~key:(k, _v1) ~data:v2 -> k, v2)
  ;;

  let do_range_restrict orig_data data ~key_range ~rank_range =
    let num_filtered_rows = Incr_collated_map.length data in
    let data, count_before_key_rank =
      do_key_range_restrict data ~key_range ~orig_map:orig_data
    in
    let data, count_before_range_rank =
      do_rank_range_restrict_and_rank data ~rank_range
    in
    let data = do_to_pos_map data in
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
  ;;

  let collate
        (type k v cmp filter order)
        ?(operation_order = `Sort_first)
        ~filter_equal
        ~order_equal
        ~(filter_to_predicate : filter -> _)
        ~(order_to_compare : order -> _)
        (data : (k, v, cmp) Map.t Incr.t)
        (collate : (k, filter, order) Collate.t Incr.t)
    : (k, v) Collated.t Incr.t
    =
    let%bind map_comparator = Incr.freeze (data >>| Map.comparator) in
    let%pattern_bind { key_range; rank_range; filter; order } = collate in
    let filter = with_cutoff filter ~equal:filter_equal in
    let order = with_cutoff order ~equal:order_equal in
    let orig_data = data in
    match operation_order with
    | `Filter_first ->
      let%bind filter = filter in
      let predicate = filter_to_predicate filter in
      let data = do_filter data ~predicate in
      let%bind order = order in
      let compare = order_to_compare order in
      let (T custom_comparator) = comparator_of_compare ~map_comparator compare in
      let data = do_sort data ~map_comparator ~custom_comparator in
      do_range_restrict orig_data data ~key_range ~rank_range
    | `Sort_first ->
      let%bind order = order in
      let compare = order_to_compare order in
      let (T custom_comparator) = comparator_of_compare ~map_comparator compare in
      let data = do_sort data ~map_comparator ~custom_comparator in
      let%bind filter = filter in
      let predicate = filter_to_predicate filter in
      let data = do_filter_sorted data ~predicate in
      do_range_restrict orig_data data ~key_range ~rank_range
  ;;

  module With_caching = struct
    module Range_memoize_bucket = Range_memoize_bucket

    let collate_and_maybe_fold__sort_first
          (type k v cmp filter order fold_result)
          ~filter_equal
          ~order_equal
          ?(order_cache_params =
            Store_params.alist_based__lru ~equal:order_equal ~max_size:10)
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
          (data : (k, v, cmp) Map.t Incr.t)
          (collate : (k, filter, order) Collate.t Incr.t)
      : ((k, v) Collated.t * fold_result) Incr.t
      =
      let cache_sorted = Store.create order_cache_params in
      let cache_sorted_filtered = Store.create order_filter_cache_params in
      let cache_sorted_filtered_ranked = Store.create order_filter_range_cache_params in
      let%bind map_comparator = Incr.freeze (data >>| Map.comparator) in
      let%pattern_bind { key_range; rank_range; filter; order } = collate in
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
      let scope = Incr.Scope.current () in
      let in_scope f = Incr.Scope.within scope ~f in
      let%bind order = order
      and filter = filter
      and range_bucket = range_bucket in
      (* This line causes the computation below to always be executed. This is fine,
         as it consists only of cache lookups, which are cheap. And we want to execute
         them to get more accurate LRU caches & hooks behaviour. *)
      let never_cutoff = return () in
      Incremental.set_cutoff never_cutoff Incremental.Cutoff.never;
      let%bind () = never_cutoff in
      let compare = order_to_compare order in
      let predicate = filter_to_predicate filter in
      let do_range ~sorted ~sorted_filtered ~fold_result =
        let sorted_filtered_ranked =
          in_scope (fun () ->
            let (Incr_collated_map.T sorted_filtered) = sorted_filtered in
            do_range_restrict orig_data sorted_filtered ~key_range ~rank_range)
        in
        Store.add
          cache_sorted_filtered_ranked
          ~key:(order, filter, range_bucket)
          ~value:(sorted, sorted_filtered, fold_result, sorted_filtered_ranked);
        Incr.both sorted_filtered_ranked fold_result
      in
      let do_filter_range ~sorted =
        let sorted_filtered : _ Incr_collated_map.packed =
          let (Incr_collated_map.T sorted) = sorted in
          in_scope (fun () -> do_filter_sorted sorted ~predicate) |> T
        in
        let fold_result =
          match fold_action with
          | Fold fold_params ->
            let (T sorted_filtered) = sorted_filtered in
            in_scope (fun () -> do_fold sorted_filtered fold_params)
          | Don't_fold -> in_scope (fun () -> Incr.return ())
        in
        Store.add
          cache_sorted_filtered
          ~key:(order, filter)
          ~value:(sorted, sorted_filtered, fold_result);
        do_range ~sorted ~sorted_filtered ~fold_result
      in
      let do_sort_filter_range () =
        let (T custom_comparator) = comparator_of_compare ~map_comparator compare in
        let sorted : _ Incr_collated_map.packed =
          in_scope (fun () -> do_sort data ~map_comparator ~custom_comparator) |> T
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
        -> Incr.both sfr fr
      | Some s, Some (s', sf, fr), _ when phys_equal s s' ->
        do_range ~sorted:s ~sorted_filtered:sf ~fold_result:fr
      | Some sorted, _, _ -> do_filter_range ~sorted
      | None, _, _ -> do_sort_filter_range ()
    ;;

    let collate__sort_first
          (type k v cmp filter order)
          ~filter_equal
          ~order_equal
          ?order_cache_params
          ?order_filter_cache_params
          ?order_filter_range_cache_params
          ?range_memoize_bucket_size
          ~(filter_to_predicate : filter -> _)
          ~(order_to_compare : order -> _)
          (data : (k, v, cmp) Map.t Incr.t)
          (collate : (k, filter, order) Collate.t Incr.t)
      : (k, v) Collated.t Incr.t
      =
      let%pattern_map collated, () =
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
      in
      collated
    ;;

    module Fold = struct
      include Fold_params

      let create ?(revert_to_init_when_empty = true) ~init ~add ?update ~remove () =
        { init; add; update; remove; revert_to_init_when_empty }
      ;;
    end

    let collate_and_fold__sort_first
          (type k v cmp filter order fold_result)
          ~filter_equal
          ~order_equal
          ?order_cache_params
          ?order_filter_cache_params
          ?order_filter_range_cache_params
          ?range_memoize_bucket_size
          ~(filter_to_predicate : filter -> _)
          ~(order_to_compare : order -> _)
          ~(fold : (k, v, fold_result) Fold_params.t)
          (data : (k, v, cmp) Map.t Incr.t)
          (collate : (k, filter, order) Collate.t Incr.t)
      : ((k, v) Collated.t * fold_result) Incr.t
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
end
