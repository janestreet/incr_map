(** Functions for using maps efficiently within Incremental.  The goal of the algorithms
    here is to do work on the output of the computation proportional to the amount of work
    done on the input.  i.e., [k] modifications to the input map for some computation will
    result in [k] modifications to the output map.  The changes to the input map are
    typically computed using [Map.symmetric_diff].

    Unless stated otherwise, the non-incremental semantics of these functions (i.e..,
    ignoring performance) is the same as the corresponding function in Core_kernel's [Map]
    module.
*)

open! Core_kernel

module Make (Incr: Incremental_kernel.Incremental.S_without_times) : sig

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
  *)
  val unordered_fold
    :  ?data_equal:('v -> 'v -> bool)
    -> ?update:(key:'k -> old_data:'v -> new_data:'v -> 'acc -> 'acc)
    -> ('k, 'v, 'cmp) Map.t Incr.t
    -> init:'acc
    -> add:(key:'k -> data:'v -> 'acc -> 'acc)
    -> remove:(key:'k -> data:'v -> 'acc -> 'acc)
    -> 'acc Incr.t

  (** Like [merge] in [Base.Map.merge]. Note that [f] is called at most once per key in
      any given stabilization. *)
  val merge
    :  ?data_equal_left:('v1 -> 'v1 -> bool)
    -> ?data_equal_right:('v2 -> 'v2 -> bool)
    -> ('k, 'v1, 'cmp) Map.t Incr.t
    -> ('k, 'v2, 'cmp) Map.t Incr.t
    -> f:(key:'k
          -> [ `Left of 'v1
             | `Right of 'v2
             | `Both of ('v1 * 'v2) ]
          -> 'v3 option)
    -> ( 'k, 'v3, 'cmp) Map.t Incr.t

  (** This is the "easy" version of [map_join] *)
  val flatten : ('k, 'v Incr.t, 'cmp) Map.t -> ('k, 'v, 'cmp) Map.t Incr.t

  (** The non-incremental semantics of this function is the identity function.  Its
      purpose is to collapse the extra level of incrementality at the level of the data of
      the map.*)
  val join
    :  ('k, 'v Incr.t, 'cmp) Map.t Incr.t
    -> ('k, 'v, 'cmp) Map.t Incr.t

  (** [subrange map (min, max)] constructs an incremental submap that includes all of the
      keys and data from [map] between [min] and [max], inclusive, and none of the keys
      outside the range.

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
    -> ('k, 'v, 'cmp) Map.t Incr.t
    -> ('k * 'k) option Incr.t
    -> ('k, 'v, 'cmp) Map.t Incr.t

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
    type ('k, 'v, 'cmp) t

    (** Create the lookup structure on an incremental map. *)
    val create
      :  ?data_equal:('v -> 'v -> bool)
      -> ('k, 'v, 'cmp) Map.t Incr.t
      -> comparator:('k, 'cmp) Comparator.t
      -> ('k, 'v, 'cmp) t

    (** Create a node which performs [Map.find] on the input map.

        [find (create incr_map) key] should be equivalent to [Incr.map ~f:(fun m ->
        Map.find m key) incr_map], but when you call [find] many times for a single
        [create] the nodes should update more efficiently in stabilisation when [incr_map]
        changes in a way which can be efficiently diffed.

        This will re-use existing nodes when it can, but will not always do so.
    *)
    val find : ('k, 'v, _) t -> 'k -> 'v option Incr.t

    module For_debug : sig
      val sexp_of_t : ('k -> Sexp.t) -> ('v -> Sexp.t) -> ('k, 'v, 'cmp) t -> Sexp.t
    end
  end
end




