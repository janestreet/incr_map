open! Core_kernel

(** [S_gen] is the type of the module returned by [Incr_map.Make].  It is a specialization
    of the interface of [Incr_map], with:

    - the ['w] state_witness type parameter removed
    - the [Incremental.State.t] argument removed

    The comments for components of [S_gen] are in [module type Incr_map] below. *)
module type S_gen = sig
  module Incr : sig
    type 'a t

    module Cutoff : sig
      type 'a t
    end
  end

  val of_set : ('k, 'cmp) Set.t Incr.t -> ('k, unit, 'cmp) Map.t Incr.t

  val filter_mapi
    :  ?data_equal:('v1 -> 'v1 -> bool)
    -> ('k, 'v1, 'cmp) Map.t Incr.t
    -> f:(key:'k -> data:'v1 -> 'v2 option)
    -> ('k, 'v2, 'cmp) Map.t Incr.t

  val mapi
    :  ?data_equal:('v1 -> 'v1 -> bool)
    -> ('k, 'v1, 'cmp) Map.t Incr.t
    -> f:(key:'k -> data:'v1 -> 'v2)
    -> ('k, 'v2, 'cmp) Map.t Incr.t

  val filter_map
    :  ?data_equal:('v1 -> 'v1 -> bool)
    -> ('k, 'v1, 'cmp) Map.t Incr.t
    -> f:('v1 -> 'v2 option)
    -> ('k, 'v2, 'cmp) Map.t Incr.t

  val map
    :  ?data_equal:('v1 -> 'v1 -> bool)
    -> ('k, 'v1, 'cmp) Map.t Incr.t
    -> f:('v1 -> 'v2)
    -> ('k, 'v2, 'cmp) Map.t Incr.t

  val filter_mapi'
    :  ?cutoff:'v1 Incr.Cutoff.t
    -> ?data_equal:('v1 -> 'v1 -> bool)
    -> ('k, 'v1, 'cmp) Map.t Incr.t
    -> f:(key:'k -> data:'v1 Incr.t -> 'v2 option Incr.t)
    -> ('k, 'v2, 'cmp) Map.t Incr.t

  val mapi'
    :  ?cutoff:'v1 Incr.Cutoff.t
    -> ?data_equal:('v1 -> 'v1 -> bool)
    -> ('k, 'v1, 'cmp) Map.t Incr.t
    -> f:(key:'k -> data:'v1 Incr.t -> 'v2 Incr.t)
    -> ('k, 'v2, 'cmp) Map.t Incr.t

  val filter_map'
    :  ?cutoff:'v1 Incr.Cutoff.t
    -> ?data_equal:('v1 -> 'v1 -> bool)
    -> ('k, 'v1, 'cmp) Map.t Incr.t
    -> f:('v1 Incr.t -> 'v2 option Incr.t)
    -> ('k, 'v2, 'cmp) Map.t Incr.t

  val map'
    :  ?cutoff:'v1 Incr.Cutoff.t
    -> ?data_equal:('v1 -> 'v1 -> bool)
    -> ('k, 'v1, 'cmp) Map.t Incr.t
    -> f:('v1 Incr.t -> 'v2 Incr.t)
    -> ('k, 'v2, 'cmp) Map.t Incr.t

  val partition_mapi
    :  ?data_equal:('v1 -> 'v1 -> bool)
    -> ('k, 'v1, 'cmp) Map.t Incr.t
    -> f:(key:'k -> data:'v1 -> ('v2, 'v3) Either.t)
    -> (('k, 'v2, 'cmp) Map.t * ('k, 'v3, 'cmp) Map.t) Incr.t

  val unordered_fold
    :  ?data_equal:('v -> 'v -> bool)
    -> ?update:(key:'k -> old_data:'v -> new_data:'v -> 'acc -> 'acc)
    -> ?specialized_initial:(init:'acc -> ('k, 'v, 'cmp) Map.t -> 'acc)
    -> ('k, 'v, 'cmp) Map.t Incr.t
    -> init:'acc
    -> add:(key:'k -> data:'v -> 'acc -> 'acc)
    -> remove:(key:'k -> data:'v -> 'acc -> 'acc)
    -> 'acc Incr.t

  val merge
    :  ?data_equal_left:('v1 -> 'v1 -> bool)
    -> ?data_equal_right:('v2 -> 'v2 -> bool)
    -> ('k, 'v1, 'cmp) Map.t Incr.t
    -> ('k, 'v2, 'cmp) Map.t Incr.t
    -> f:(key:'k -> [ `Left of 'v1 | `Right of 'v2 | `Both of 'v1 * 'v2 ] -> 'v3 option)
    -> ('k, 'v3, 'cmp) Map.t Incr.t

  val merge'
    :  ?cutoff:[ `Both of 'v1 * 'v2 | `Left of 'v1 | `Right of 'v2 ] Incr.Cutoff.t
    -> ?data_equal_left:('v1 -> 'v1 -> bool)
    -> ?data_equal_right:('v2 -> 'v2 -> bool)
    -> ('k, 'v1, 'cmp) Map.t Incr.t
    -> ('k, 'v2, 'cmp) Map.t Incr.t
    -> f:
         (key:'k
          -> [ `Left of 'v1 | `Right of 'v2 | `Both of 'v1 * 'v2 ] Incr.t
          -> 'v3 option Incr.t)
    -> ('k, 'v3, 'cmp) Map.t Incr.t

  val flatten : ('k, 'v Incr.t, 'cmp) Map.t -> ('k, 'v, 'cmp) Map.t Incr.t
  val join : ('k, 'v Incr.t, 'cmp) Map.t Incr.t -> ('k, 'v, 'cmp) Map.t Incr.t

  val separate
    :  ('k, 'v, 'cmp) Map.t Incr.t
    -> data_equal:('v -> 'v -> bool)
    -> ('k, 'v Incr.t, 'cmp) Map.t Incr.t

  val keys : ('k, 'v, 'c) Map.t Incr.t -> ('k, 'c) Set.t Incr.t
  val rank : ('k, 'v, 'cmp) Base.Map.t Incr.t -> 'k Incr.t -> int option Incr.t

  val subrange
    :  ?data_equal:('v -> 'v -> bool)
    -> ('k, 'v, 'cmp) Map.t Incr.t
    -> ('k Maybe_bound.As_lower_bound.t * 'k Maybe_bound.As_upper_bound.t) option Incr.t
    -> ('k, 'v, 'cmp) Map.t Incr.t

  val subrange_by_rank
    :  ?data_equal:('v -> 'v -> bool)
    -> ('k, 'v, 'cmp) Map.t Incr.t
    -> (int Maybe_bound.As_lower_bound.t * int Maybe_bound.As_upper_bound.t) Incr.t
    -> ('k, 'v, 'cmp) Map.t Incr.t

  val index_by
    :  ('inner_key, 'v, 'inner_cmp) Map.t Incr.t
    -> comparator:('outer_key, 'outer_cmp) Map.comparator
    -> index:('v -> 'outer_key option)
    -> ('outer_key, ('inner_key, 'v, 'inner_cmp) Map.t, 'outer_cmp) Map.t Incr.t

  val unordered_fold_nested_maps
    :  ?data_equal:('v -> 'v -> bool)
    -> ?update:
         (outer_key:'outer_key
          -> inner_key:'inner_key
          -> old_data:'v
          -> new_data:'v
          -> 'acc
          -> 'acc)
    -> ('outer_key, ('inner_key, 'v, 'inner_cmp) Map.t, 'outer_cmp) Map.t Incr.t
    -> init:'acc
    -> add:(outer_key:'outer_key -> inner_key:'inner_key -> data:'v -> 'acc -> 'acc)
    -> remove:(outer_key:'outer_key -> inner_key:'inner_key -> data:'v -> 'acc -> 'acc)
    -> 'acc Incr.t

  val transpose
    :  ?data_equal:('v -> 'v -> bool)
    -> ('k2, 'k2_cmp) Map.comparator
    -> ('k1, ('k2, 'v, 'k2_cmp) Map.t, 'k1_cmp) Map.t Incr.t
    -> ('k2, ('k1, 'v, 'k1_cmp) Map.t, 'k2_cmp) Map.t Incr.t

  val collapse
    :  ?data_equal:('v -> 'v -> bool)
    -> ('outer_key, ('inner_key, 'v, 'inner_cmp) Map.t, 'outer_cmp) Map.t Incr.t
    -> comparator:('inner_key, 'inner_cmp) Map.comparator
    -> ( 'outer_key * 'inner_key
       , 'v
       , ('outer_cmp, 'inner_cmp) Tuple2.comparator_witness )
         Map.t
         Incr.t

  val counti
    :  ?data_equal:('v -> 'v -> bool)
    -> ('k, 'v, _) Map.t Incr.t
    -> f:(key:'k -> data:'v -> bool)
    -> int Incr.t

  val count
    :  ?data_equal:('v -> 'v -> bool)
    -> (_, 'v, _) Map.t Incr.t
    -> f:('v -> bool)
    -> int Incr.t

  val for_alli
    :  ?data_equal:('v -> 'v -> bool)
    -> ('k, 'v, _) Map.t Incr.t
    -> f:(key:'k -> data:'v -> bool)
    -> bool Incr.t

  val for_all
    :  ?data_equal:('v -> 'v -> bool)
    -> (_, 'v, _) Map.t Incr.t
    -> f:('v -> bool)
    -> bool Incr.t

  val existsi
    :  ?data_equal:('v -> 'v -> bool)
    -> ('k, 'v, _) Map.t Incr.t
    -> f:(key:'k -> data:'v -> bool)
    -> bool Incr.t

  val exists
    :  ?data_equal:('v -> 'v -> bool)
    -> (_, 'v, _) Map.t Incr.t
    -> f:('v -> bool)
    -> bool Incr.t

  module Lookup : sig
    type ('k, 'v, 'cmp) t

    val create
      :  ?data_equal:('v -> 'v -> bool)
      -> ('k, 'v, 'cmp) Map.t Incr.t
      -> comparator:('k, 'cmp) Comparator.t
      -> ('k, 'v, 'cmp) t

    val find : ('k, 'v, _) t -> 'k -> 'v option Incr.t

    module M (K : sig
        type t
        type comparator_witness
      end) : sig
      type nonrec 'v t = (K.t, 'v, K.comparator_witness) t
    end

    module For_debug : sig
      val sexp_of_t : ('k -> Sexp.t) -> ('v -> Sexp.t) -> ('k, 'v, 'cmp) t -> Sexp.t
    end
  end

  module For_testing : sig
    val find_key_range_linear
      :  from:int
      -> to_:int
      -> ('a, 'b, 'c) Base.Map.t
      -> ('a * 'a option) option
  end
end

module type Incr_map = sig
  (** Functions for using maps efficiently within Incremental.  The goal of the algorithms
      here is to do work on the output of the computation proportional to the amount of
      work done on the input.  i.e., [k] modifications to the input map for some
      computation will result in [k] modifications to the output map.  The changes to the
      input map are typically computed using [Map.symmetric_diff].

      Unless stated otherwise, the non-incremental semantics of these functions (i.e..,
      ignoring performance) is the same as the corresponding function in Core_kernel's
      [Map] module.  *)

  val of_set
    :  (('k, 'cmp) Set.t, 'w) Incremental.t
    -> (('k, unit, 'cmp) Map.t, 'w) Incremental.t

  val filter_mapi
    :  ?data_equal:('v1 -> 'v1 -> bool)
    -> (('k, 'v1, 'cmp) Map.t, 'w) Incremental.t
    -> f:(key:'k -> data:'v1 -> 'v2 option)
    -> (('k, 'v2, 'cmp) Map.t, 'w) Incremental.t

  val mapi
    :  ?data_equal:('v1 -> 'v1 -> bool)
    -> (('k, 'v1, 'cmp) Map.t, 'w) Incremental.t
    -> f:(key:'k -> data:'v1 -> 'v2)
    -> (('k, 'v2, 'cmp) Map.t, 'w) Incremental.t

  val filter_map
    :  ?data_equal:('v1 -> 'v1 -> bool)
    -> (('k, 'v1, 'cmp) Map.t, 'w) Incremental.t
    -> f:('v1 -> 'v2 option)
    -> (('k, 'v2, 'cmp) Map.t, 'w) Incremental.t

  val map
    :  ?data_equal:('v1 -> 'v1 -> bool)
    -> (('k, 'v1, 'cmp) Map.t, 'w) Incremental.t
    -> f:('v1 -> 'v2)
    -> (('k, 'v2, 'cmp) Map.t, 'w) Incremental.t

  val filter_mapi'
    :  ?cutoff:'v1 Incremental.Cutoff.t
    -> ?data_equal:('v1 -> 'v1 -> bool)
    -> (('k, 'v1, 'cmp) Map.t, 'w) Incremental.t
    -> f:(key:'k -> data:('v1, 'w) Incremental.t -> ('v2 option, 'w) Incremental.t)
    -> (('k, 'v2, 'cmp) Map.t, 'w) Incremental.t

  val map'
    :  ?cutoff:'v1 Incremental.Cutoff.t
    -> ?data_equal:('v1 -> 'v1 -> bool)
    -> (('k, 'v1, 'cmp) Map.t, 'w) Incremental.t
    -> f:(('v1, 'w) Incremental.t -> ('v2, 'w) Incremental.t)
    -> (('k, 'v2, 'cmp) Map.t, 'w) Incremental.t

  val filter_map'
    :  ?cutoff:'v1 Incremental.Cutoff.t
    -> ?data_equal:('v1 -> 'v1 -> bool)
    -> (('k, 'v1, 'cmp) Map.t, 'w) Incremental.t
    -> f:(('v1, 'w) Incremental.t -> ('v2 option, 'w) Incremental.t)
    -> (('k, 'v2, 'cmp) Map.t, 'w) Incremental.t

  val mapi'
    :  ?cutoff:'v1 Incremental.Cutoff.t
    -> ?data_equal:('v1 -> 'v1 -> bool)
    -> (('k, 'v1, 'cmp) Map.t, 'w) Incremental.t
    -> f:(key:'k -> data:('v1, 'w) Incremental.t -> ('v2, 'w) Incremental.t)
    -> (('k, 'v2, 'cmp) Map.t, 'w) Incremental.t

  val partition_mapi
    :  ?data_equal:('v1 -> 'v1 -> bool)
    -> (('k, 'v1, 'cmp) Map.t, 'w) Incremental.t
    -> f:(key:'k -> data:'v1 -> ('v2, 'v3) Either.t)
    -> (('k, 'v2, 'cmp) Map.t * ('k, 'v3, 'cmp) Map.t, 'w) Incremental.t

  (** [unordered_fold i ~init ~add ~remove] constructs a more incremental version of:

      {[
        let%map m = i in
        Map.fold m ~init ~f:add
      ]}

      assuming that [remove] is the inverse of [add], and that the operations for
      different keys can be performed in any order. Note that [data_equal] defaults
      to [phys_equal], but a more precise equality can be provided instead.

      When the data for a key updates, by default [remove] is called on the old data
      and then [add] is called on the new data.
      [update] provides an alternative single function to call each time a key's data
      updates, and can be used to improve efficiency.

      For the initial computation, by default [add] is called on all the elements in the
      map. As this can be inefficient, [specialized_initial] can be provided to perform
      the computation in a more effective way.
  *)
  val unordered_fold
    :  ?data_equal:('v -> 'v -> bool)
    -> ?update:(key:'k -> old_data:'v -> new_data:'v -> 'acc -> 'acc)
    -> ?specialized_initial:(init:'acc -> ('k, 'v, 'cmp) Map.t -> 'acc)
    -> (('k, 'v, 'cmp) Map.t, 'w) Incremental.t
    -> init:'acc
    -> add:(key:'k -> data:'v -> 'acc -> 'acc)
    -> remove:(key:'k -> data:'v -> 'acc -> 'acc)
    -> ('acc, 'w) Incremental.t


  (** Like [merge] in [Base.Map.merge]. Note that [f] is called at most once per key in
      any given stabilization. *)
  val merge
    :  ?data_equal_left:('v1 -> 'v1 -> bool)
    -> ?data_equal_right:('v2 -> 'v2 -> bool)
    -> (('k, 'v1, 'cmp) Map.t, 'w) Incremental.t
    -> (('k, 'v2, 'cmp) Map.t, 'w) Incremental.t
    -> f:(key:'k -> [ `Left of 'v1 | `Right of 'v2 | `Both of 'v1 * 'v2 ] -> 'v3 option)
    -> (('k, 'v3, 'cmp) Map.t, 'w) Incremental.t

  (** Like [merge], but operating using incremental nodes. This is a good use case for
      [ppx_pattern_bind]. *)
  val merge'
    :  ?cutoff:[ `Left of 'v1 | `Right of 'v2 | `Both of 'v1 * 'v2 ] Incremental.Cutoff.t
    -> ?data_equal_left:('v1 -> 'v1 -> bool)
    -> ?data_equal_right:('v2 -> 'v2 -> bool)
    -> (('k, 'v1, 'cmp) Map.t, 'w) Incremental.t
    -> (('k, 'v2, 'cmp) Map.t, 'w) Incremental.t
    -> f:
         (key:'k
          -> ([ `Left of 'v1 | `Right of 'v2 | `Both of 'v1 * 'v2 ], 'w) Incremental.t
          -> ('v3 option, 'w) Incremental.t)
    -> (('k, 'v3, 'cmp) Map.t, 'w) Incremental.t

  (** This is the "easy" version of [join] *)
  val flatten
    :  'w Incremental.State.t
    -> ('k, ('v, 'w) Incremental.t, 'cmp) Map.t
    -> (('k, 'v, 'cmp) Map.t, 'w) Incremental.t

  (** The non-incremental semantics of this function is the identity function.  Its
      purpose is to collapse the extra level of incrementality at the level of the data of
      the map.*)
  val join
    :  (('k, ('v, 'w) Incremental.t, 'cmp) Map.t, 'w) Incremental.t
    -> (('k, 'v, 'cmp) Map.t, 'w) Incremental.t

  val separate
    :  (('k, 'v, 'cmp) Map.t, 'w) Incremental.t
    -> data_equal:('v -> 'v -> bool)
    -> (('k, ('v, 'w) Incremental.t, 'cmp) Map.t, 'w) Incremental.t

  val keys : (('k, 'v, 'c) Map.t, 'w) Incremental.t -> (('k, 'c) Set.t, 'w) Incremental.t

  (** Computes the [rank] of a key (given incrementally) inside of a map (also
      incremental).  The traditional [Map.rank] function is O(n), and this incremental
      rank function has the following performance characteristics:

      definitions:
      n : the size of the map
      r : the time to compute [Map.symmetric_diff] between the two maps
      k : the change in rank of the key between two stabilizations

      note that [r] and [k] are _much_ smaller than [n] for most practical purposes

      - O(log n) when the key is not in the map.
        This takes precedence over other every other scenario.
      - O(n) on the initial stabilization
      - O(n) when the key transitions from not being in the map to being in the map
      - O(log n + r) when the map changes
      - O(log n + k) when the key changes
      - O(log n + r + k) when both key and map change *)
  val rank
    :  (('k, 'v, 'cmp) Base.Map.t, 'state_witness) Incremental.t
    -> ('k, 'state_witness) Incremental.t
    -> (int option, 'state_witness) Incremental.t

  (** [subrange map (min, max)] constructs an incremental submap that includes all of the
      keys and data from [map] between [min] and [max], and none of the keys outside the
      range.

      [subrange map None] is the empty map. [range] being [None] means no elements are
      chosen.

      Note that incremental changes have a runtime of O((k + m) log n) where k is the size
      of the changes to the underlying map and m is the size of the changes to the
      elements contained by the range. The complexity of the initial computation is the
      same as the incremental computation, with some simplification. k = 0 because we have
      not made any changes to the underlying map yet, and m equals the size of the range,
      because the initial range is empty. *)
  val subrange
    :  ?data_equal:('v -> 'v -> bool)
    -> (('k, 'v, 'cmp) Map.t, 'w) Incremental.t
    -> ( ('k Maybe_bound.As_lower_bound.t * 'k Maybe_bound.As_upper_bound.t) option
       , 'w )
         Incremental.t
    -> (('k, 'v, 'cmp) Map.t, 'w) Incremental.t

  (** [subrange_by_rank map (s, e)] constructs an incremental submap that includes (e-s+1)
      keys between s-th and e-th, inclusive.

      If s is greater or equal to map length, the result is empty.
      If e is greater or equal to map length, the result contains keys from s-th to the
      last one.

      Raises for invalid indices - s < 0 or e < s.

      Runtime of the initial computation is O(min(e, n-s) + log(n)), i.e. linear,
      but optimized for ranges close to beginning or end.

      Runtime of the incremental computation is O(log(n) + k + (m+m') * log(n)) where:
      - k is the size of the diff
      - m is the total impact of map changes on the range, bounded by k (e.g. if we add
        1001 keys and remove 1000 below s, then m = 1)
      - m' = O( |new s - old s| + |new e - old e| ).
  *)
  val subrange_by_rank
    :  ?data_equal:('v -> 'v -> bool)
    -> (('k, 'v, 'cmp) Map.t, 'w) Incremental.t
    -> ( int Maybe_bound.As_lower_bound.t * int Maybe_bound.As_upper_bound.t
       , 'w )
         Incremental.t
    -> (('k, 'v, 'cmp) Map.t, 'w) Incremental.t

  (** [index_by map ~comparator ~index] constructs an incremental map-of-maps where each
      key-data pair of the input map is present in one (or none) of the inner maps.
      [index] specifies the outer map key under which each original key-data pair is
      found.

      All of the resulting inner maps are guaranteed to be non-empty.

      An all-at-once version of [index_by] would look like:

      {[
        let index_by map ~comparator ~index =
          Map.to_alist map
          |> List.filter_map ~f:(fun (key, data) ->
            match index data with
            | None -> None
            | Some index -> index, (key, data))
          |> Map.of_alist_multi comparator
          |> Map.map ~f:(Map.of_alist_exn (Map.comparator_s map))
        ;;
      ]} *)
  val index_by
    :  (('inner_key, 'v, 'inner_cmp) Map.t, 'w) Incremental.t
    -> comparator:('outer_key, 'outer_cmp) Map.comparator
    -> index:('v -> 'outer_key option)
    -> ( ('outer_key, ('inner_key, 'v, 'inner_cmp) Map.t, 'outer_cmp) Map.t
       , 'w )
         Incremental.t

  val unordered_fold_nested_maps
    :  ?data_equal:('v -> 'v -> bool)
    -> ?update:
         (outer_key:'outer_key
          -> inner_key:'inner_key
          -> old_data:'v
          -> new_data:'v
          -> 'acc
          -> 'acc)
    -> ( ('outer_key, ('inner_key, 'v, 'inner_cmp) Map.t, 'outer_cmp) Map.t
       , 'w )
         Incremental.t
    -> init:'acc
    -> add:(outer_key:'outer_key -> inner_key:'inner_key -> data:'v -> 'acc -> 'acc)
    -> remove:(outer_key:'outer_key -> inner_key:'inner_key -> data:'v -> 'acc -> 'acc)
    -> ('acc, 'w) Incremental.t

  val transpose
    :  ?data_equal:('v -> 'v -> bool)
    -> ('k2, 'k2_cmp) Map.comparator
    -> (('k1, ('k2, 'v, 'k2_cmp) Map.t, 'k1_cmp) Map.t, 'w) Incremental.t
    -> (('k2, ('k1, 'v, 'k1_cmp) Map.t, 'k2_cmp) Map.t, 'w) Incremental.t

  val collapse
    :  ?data_equal:('v -> 'v -> bool)
    -> ( ('outer_key, ('inner_key, 'v, 'inner_cmp) Map.t, 'outer_cmp) Map.t
       , 'w )
         Incremental.t
    -> comparator:('inner_key, 'inner_cmp) Map.comparator
    -> ( ( 'outer_key * 'inner_key
         , 'v
         , ('outer_cmp, 'inner_cmp) Tuple2.comparator_witness )
           Map.t
       , 'w )
         Incremental.t

  val counti
    :  ?data_equal:('v -> 'v -> bool)
    -> (('k, 'v, _) Map.t, 'w) Incremental.t
    -> f:(key:'k -> data:'v -> bool)
    -> (int, 'w) Incremental.t

  val count
    :  ?data_equal:('v -> 'v -> bool)
    -> ((_, 'v, _) Map.t, 'w) Incremental.t
    -> f:('v -> bool)
    -> (int, 'w) Incremental.t

  val for_alli
    :  ?data_equal:('v -> 'v -> bool)
    -> (('k, 'v, _) Map.t, 'w) Incremental.t
    -> f:(key:'k -> data:'v -> bool)
    -> (bool, 'w) Incremental.t

  val for_all
    :  ?data_equal:('v -> 'v -> bool)
    -> ((_, 'v, _) Map.t, 'w) Incremental.t
    -> f:('v -> bool)
    -> (bool, 'w) Incremental.t

  val existsi
    :  ?data_equal:('v -> 'v -> bool)
    -> (('k, 'v, _) Map.t, 'w) Incremental.t
    -> f:(key:'k -> data:'v -> bool)
    -> (bool, 'w) Incremental.t

  val exists
    :  ?data_equal:('v -> 'v -> bool)
    -> ((_, 'v, _) Map.t, 'w) Incremental.t
    -> f:('v -> bool)
    -> (bool, 'w) Incremental.t

  (** [('k, 'v) Lookup.t] provides a way to lookup keys in a map which uses symmetric
      diffs to trigger updates of the lookups.

      The complexity of an update depends on:
      - [n]: the number of keys in the larger of the old/updated input map
      - [k]: the number of lookup nodes created using [find]
      - [m]: the number of elements in the symdiff of the maps
      - [symdiff(n)]: the cost of performing the symdiff on the map (m <= symdiff(n) <= n)

      Each update should cost [O(symdiff(n) + m * log k)], so this will be efficient when
      there are a lot of lookups (close to n) into a map which can be efficiently
      symdiffed (and therefore has a small number of changes also). The cost of updating
      when performing the same lookups by means of [Incr.map ~f:(fun m -> Map.find m key)]
      is [O(k * log n)].
  *)
  module Lookup : sig
    type ('k, 'v, 'cmp, 'w) t

    (** Create the lookup structure on an incremental map. *)
    val create
      :  ?data_equal:('v -> 'v -> bool)
      -> (('k, 'v, 'cmp) Map.t, 'w) Incremental.t
      -> comparator:('k, 'cmp) Comparator.t
      -> ('k, 'v, 'cmp, 'w) t

    (** Create a node which performs [Map.find] on the input map.

        [find (create incr_map) key] should be equivalent to [Incr.map ~f:(fun m ->
        Map.find m key) incr_map], but when you call [find] many times for a single
        [create] the nodes should update more efficiently in stabilisation when [incr_map]
        changes in a way which can be efficiently diffed.

        This will re-use existing nodes when it can, but will not always do so.
    *)
    val find : ('k, 'v, _, 'w) t -> 'k -> ('v option, 'w) Incremental.t

    (** A convenient way to refer to the type for a given key. *)
    module M (K : sig
        type t
        type comparator_witness
      end) : sig
      type nonrec ('v, 'w) t = (K.t, 'v, K.comparator_witness, 'w) t
    end

    module For_debug : sig
      val sexp_of_t : ('k -> Sexp.t) -> ('v -> Sexp.t) -> ('k, 'v, 'cmp, _) t -> Sexp.t
    end
  end

  module For_testing : sig
    val find_key_range_linear
      :  from:int
      -> to_:int
      -> ('a, 'b, 'c) Base.Map.t
      -> ('a * 'a option) option
  end

  module type S_gen = S_gen

  module type S = sig
    type state_witness

    include
      S_gen
      with type 'a Incr.t = ('a, state_witness) Incremental.t
       and type 'a Incr.Cutoff.t = 'a Incremental.Cutoff.t
       and type ('k, 'v, 'cmp) Lookup.t = ('k, 'v, 'cmp, state_witness) Lookup.t
  end

  module Make (Incr : Incremental.S) :
    S with type state_witness := Incr.state_witness and module Incr := Incr
end

