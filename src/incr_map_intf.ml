open! Core

module Instrumentation = struct
  (** All [Incr_map] functions take an optional [instrumentation] parameter that has type
      [Instrumentation.t]. A value of this type is a record containing a function which is
      polymorphic over a universally-quantified type ['a]. This function is passed a
      [unit -> 'a] function, which must be immediately executed, and the result of which
      must be returned.

      The function passed to the instrumentor will be doing the bulk of the work for the
      [Incr_map] function in question (usually a [Map.fold_symmetric_diff]).

      You may want to use the Instrumentation API to assist in performance profiling like
      so:

      {[
        let profile name =
          { Incr_map.Instrumentation.f =
              (fun f ->
                let before = Time.now () in
                let r = f () in
                let after = Time.now () in
                let delta = Time.sub after before in
                printf "%s took %s" name (Time.Span.to_string_hum delta);
                r)
          }
        ;;

        Incr_map.map ~instrumentation:(profile "foo") ~f:map_foo
      ]} *)

  type t = { f : 'a. (unit -> 'a) -> 'a } [@@unboxed]
end

(** [S_gen] is the type of the module returned by [Incr_map.Make]. It is a specialization
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

  module Instrumentation = Instrumentation

  val of_set
    :  ?instrumentation:Instrumentation.t
    -> ('k, 'cmp) Set.t Incr.t
    -> ('k, unit, 'cmp) Map.t Incr.t

  val filter_mapi
    :  ?instrumentation:Instrumentation.t
    -> ?data_equal:('v1 -> 'v1 -> bool)
    -> ('k, 'v1, 'cmp) Map.t Incr.t
    -> f:(key:'k -> data:'v1 -> 'v2 option)
    -> ('k, 'v2, 'cmp) Map.t Incr.t

  val mapi
    :  ?instrumentation:Instrumentation.t
    -> ?data_equal:('v1 -> 'v1 -> bool)
    -> ('k, 'v1, 'cmp) Map.t Incr.t
    -> f:(key:'k -> data:'v1 -> 'v2)
    -> ('k, 'v2, 'cmp) Map.t Incr.t

  val filter_map
    :  ?instrumentation:Instrumentation.t
    -> ?data_equal:('v1 -> 'v1 -> bool)
    -> ('k, 'v1, 'cmp) Map.t Incr.t
    -> f:('v1 -> 'v2 option)
    -> ('k, 'v2, 'cmp) Map.t Incr.t

  val map
    :  ?instrumentation:Instrumentation.t
    -> ?data_equal:('v1 -> 'v1 -> bool)
    -> ('k, 'v1, 'cmp) Map.t Incr.t
    -> f:('v1 -> 'v2)
    -> ('k, 'v2, 'cmp) Map.t Incr.t

  val filter_mapi'
    :  ?instrumentation:Instrumentation.t
    -> ?cutoff:'v1 Incr.Cutoff.t
    -> ?data_equal:('v1 -> 'v1 -> bool)
    -> ('k, 'v1, 'cmp) Map.t Incr.t
    -> f:(key:'k -> data:'v1 Incr.t -> 'v2 option Incr.t)
    -> ('k, 'v2, 'cmp) Map.t Incr.t

  val mapi'
    :  ?instrumentation:Instrumentation.t
    -> ?cutoff:'v1 Incr.Cutoff.t
    -> ?data_equal:('v1 -> 'v1 -> bool)
    -> ('k, 'v1, 'cmp) Map.t Incr.t
    -> f:(key:'k -> data:'v1 Incr.t -> 'v2 Incr.t)
    -> ('k, 'v2, 'cmp) Map.t Incr.t

  val filter_map'
    :  ?instrumentation:Instrumentation.t
    -> ?cutoff:'v1 Incr.Cutoff.t
    -> ?data_equal:('v1 -> 'v1 -> bool)
    -> ('k, 'v1, 'cmp) Map.t Incr.t
    -> f:('v1 Incr.t -> 'v2 option Incr.t)
    -> ('k, 'v2, 'cmp) Map.t Incr.t

  val map'
    :  ?instrumentation:Instrumentation.t
    -> ?cutoff:'v1 Incr.Cutoff.t
    -> ?data_equal:('v1 -> 'v1 -> bool)
    -> ('k, 'v1, 'cmp) Map.t Incr.t
    -> f:('v1 Incr.t -> 'v2 Incr.t)
    -> ('k, 'v2, 'cmp) Map.t Incr.t

  val partition_mapi
    :  ?instrumentation:Instrumentation.t
    -> ?data_equal:('v1 -> 'v1 -> bool)
    -> ('k, 'v1, 'cmp) Map.t Incr.t
    -> f:(key:'k -> data:'v1 -> ('v2, 'v3) Either.t)
    -> (('k, 'v2, 'cmp) Map.t * ('k, 'v3, 'cmp) Map.t) Incr.t

  val partition_mapi'
    :  ?instrumentation:Instrumentation.t
    -> ?cutoff:'v1 Incr.Cutoff.t
    -> ?data_equal:('v1 -> 'v1 -> bool)
    -> ('k, 'v1, 'cmp) Map.t Incr.t
    -> f:(key:'k -> data:'v1 Incr.t -> ('v2, 'v3) Either.t Incr.t)
    -> (('k, 'v2, 'cmp) Map.t * ('k, 'v3, 'cmp) Map.t) Incr.t

  val unordered_fold
    :  ?instrumentation:Instrumentation.t
    -> ?data_equal:('v -> 'v -> bool)
    -> ?update:(key:'k -> old_data:'v -> new_data:'v -> 'acc -> 'acc)
    -> ?specialized_initial:(init:'acc -> ('k, 'v, 'cmp) Map.t -> 'acc)
    -> ?finalize:('acc -> 'acc)
    -> ?revert_to_init_when_empty:bool
    -> ('k, 'v, 'cmp) Map.t Incr.t
    -> init:'acc
    -> add:(key:'k -> data:'v -> 'acc -> 'acc)
    -> remove:(key:'k -> data:'v -> 'acc -> 'acc)
    -> 'acc Incr.t

  val unordered_fold_with_extra
    :  ?instrumentation:Instrumentation.t
    -> ?data_equal:('v -> 'v -> bool)
    -> ?extra_equal:('extra -> 'extra -> bool)
    -> ?update:(key:'k -> old_data:'v -> new_data:'v -> 'acc -> 'extra -> 'acc)
    -> ?specialized_initial:(init:'acc -> ('k, 'v, 'e) Map.t -> 'extra -> 'acc)
    -> ?finalize:('acc -> 'acc)
    -> ?revert_to_init_when_empty:bool
    -> ('k, 'v, 'e) Map.t Incr.t
    -> 'extra Incr.t
    -> init:'acc
    -> add:(key:'k -> data:'v -> 'acc -> 'extra -> 'acc)
    -> remove:(key:'k -> data:'v -> 'acc -> 'extra -> 'acc)
    -> extra_changed:
         (old_extra:'extra
          -> new_extra:'extra
          -> input:('k, 'v, 'e) Map.t
          -> 'acc
          -> 'acc)
    -> 'acc Incr.t

  val cutoff
    :  ?instrumentation:Instrumentation.t
    -> ('k, 'v, 'cmp) Map.t Incr.t
    -> cutoff:'v Incremental.Cutoff.t
    -> ('k, 'v, 'cmp) Map.t Incr.t

  val mapi_count
    :  ?instrumentation:Instrumentation.t
    -> ?data_equal:('v -> 'v -> bool)
    -> ('k1, 'v, 'cmp1) Map.t Incr.t
    -> comparator:('k2, 'cmp2) Comparator.Module.t
    -> f:(key:'k1 -> data:'v -> 'k2)
    -> ('k2, int, 'cmp2) Map.t Incr.t

  val map_count
    :  ?instrumentation:Instrumentation.t
    -> ?data_equal:('v -> 'v -> bool)
    -> ('k1, 'v, 'cmp1) Map.t Incr.t
    -> comparator:('k2, 'cmp2) Comparator.Module.t
    -> f:('v -> 'k2)
    -> ('k2, int, 'cmp2) Map.t Incr.t

  val mapi_min
    :  ?instrumentation:Instrumentation.t
    -> ?data_equal:('v -> 'v -> bool)
    -> ('k, 'v, _) Map.t Incr.t
    -> comparator:('r, _) Comparator.Module.t
    -> f:(key:'k -> data:'v -> 'r)
    -> 'r option Incr.t

  val mapi_max
    :  ?instrumentation:Instrumentation.t
    -> ?data_equal:('v -> 'v -> bool)
    -> ('k, 'v, _) Map.t Incr.t
    -> comparator:('r, _) Comparator.Module.t
    -> f:(key:'k -> data:'v -> 'r)
    -> 'r option Incr.t

  val map_min
    :  ?instrumentation:Instrumentation.t
    -> ?data_equal:('v -> 'v -> bool)
    -> ('k, 'v, _) Map.t Incr.t
    -> comparator:('r, _) Comparator.Module.t
    -> f:('v -> 'r)
    -> 'r option Incr.t

  val map_max
    :  ?instrumentation:Instrumentation.t
    -> ?data_equal:('v -> 'v -> bool)
    -> ('k, 'v, _) Map.t Incr.t
    -> comparator:('r, _) Comparator.Module.t
    -> f:('v -> 'r)
    -> 'r option Incr.t

  val min_value
    :  ?instrumentation:Instrumentation.t
    -> ?data_equal:('v -> 'v -> bool)
    -> ('k, 'v, _) Map.t Incr.t
    -> comparator:('v, _) Comparator.Module.t
    -> 'v option Incr.t

  val max_value
    :  ?instrumentation:Instrumentation.t
    -> ?data_equal:('v -> 'v -> bool)
    -> ('k, 'v, _) Map.t Incr.t
    -> comparator:('v, _) Comparator.Module.t
    -> 'v option Incr.t

  val mapi_bounds
    :  ?instrumentation:Instrumentation.t
    -> ?data_equal:('v -> 'v -> bool)
    -> ('k, 'v, _) Map.t Incr.t
    -> comparator:('r, _) Comparator.Module.t
    -> f:(key:'k -> data:'v -> 'r)
    -> ('r * 'r) option Incr.t

  val map_bounds
    :  ?instrumentation:Instrumentation.t
    -> ?data_equal:('v -> 'v -> bool)
    -> ('k, 'v, _) Map.t Incr.t
    -> comparator:('r, _) Comparator.Module.t
    -> f:('v -> 'r)
    -> ('r * 'r) option Incr.t

  val value_bounds
    :  ?instrumentation:Instrumentation.t
    -> ?data_equal:('v -> 'v -> bool)
    -> ('k, 'v, _) Map.t Incr.t
    -> comparator:('v, _) Comparator.Module.t
    -> ('v * 'v) option Incr.t

  val merge
    :  ?instrumentation:Instrumentation.t
    -> ?data_equal_left:('v1 -> 'v1 -> bool)
    -> ?data_equal_right:('v2 -> 'v2 -> bool)
    -> ('k, 'v1, 'cmp) Map.t Incr.t
    -> ('k, 'v2, 'cmp) Map.t Incr.t
    -> f:(key:'k -> ('v1, 'v2) Map.Merge_element.t -> 'v3 option)
    -> ('k, 'v3, 'cmp) Map.t Incr.t

  val merge_both_some
    :  ?instrumentation:Instrumentation.t
    -> ?data_equal_left:('v1 -> 'v1 -> bool)
    -> ?data_equal_right:('v2 -> 'v2 -> bool)
    -> ?out_equal:('v3 -> 'v3 -> bool)
    -> ('k, 'v1, 'cmp) Map.t Incr.t
    -> ('k, 'v2, 'cmp) Map.t Incr.t
    -> f:(key:'k -> 'v1 -> 'v2 -> 'v3)
    -> ('k, 'v3, 'cmp) Map.t Incr.t

  val merge_disjoint
    :  ?instrumentation:Instrumentation.t
    -> ?data_equal:('v -> 'v -> bool)
    -> ('k, 'v, 'cmp) Map.t Incr.t
    -> ('k, 'v, 'cmp) Map.t Incr.t
    -> ('k, 'v, 'cmp) Map.t Incr.t

  val unzip
    :  ?instrumentation:Instrumentation.t
    -> ?left_result_equal:('v1 -> 'v1 -> bool)
    -> ?right_result_equal:('v2 -> 'v2 -> bool)
    -> ('k, 'v1 * 'v2, 'cmp) Map.t Incr.t
    -> ('k, 'v1, 'cmp) Map.t Incr.t * ('k, 'v2, 'cmp) Map.t Incr.t

  val unzip_mapi
    :  ?instrumentation:Instrumentation.t
    -> ?data_equal:('v -> 'v -> bool)
    -> ?left_result_equal:('v1 -> 'v1 -> bool)
    -> ?right_result_equal:('v2 -> 'v2 -> bool)
    -> ('k, 'v, 'cmp) Map.t Incr.t
    -> f:(key:'k -> data:'v -> 'v1 * 'v2)
    -> ('k, 'v1, 'cmp) Map.t Incr.t * ('k, 'v2, 'cmp) Map.t Incr.t

  val unzip_mapi'
    :  ?instrumentation:Instrumentation.t
    -> ?cutoff:'v Incr.Cutoff.t
    -> ?data_equal:('v -> 'v -> bool)
    -> ('k, 'v, 'cmp) Map.t Incr.t
    -> f:(key:'k -> data:'v Incr.t -> 'v1 Incr.t * 'v2 Incr.t)
    -> ('k, 'v1, 'cmp) Map.t Incr.t * ('k, 'v2, 'cmp) Map.t Incr.t

  val unzip3_mapi'
    :  ?instrumentation:Instrumentation.t
    -> ?cutoff:'v Incr.Cutoff.t
    -> ?data_equal:('v -> 'v -> bool)
    -> ('k, 'v, 'cmp) Map.t Incr.t
    -> f:(key:'k -> data:'v Incr.t -> 'v1 Incr.t * 'v2 Incr.t * 'v3 Incr.t)
    -> ('k, 'v1, 'cmp) Map.t Incr.t
       * ('k, 'v2, 'cmp) Map.t Incr.t
       * ('k, 'v3, 'cmp) Map.t Incr.t

  val merge'
    :  ?instrumentation:Instrumentation.t
    -> ?cutoff:('v1, 'v2) Map.Merge_element.t Incr.Cutoff.t
    -> ?data_equal_left:('v1 -> 'v1 -> bool)
    -> ?data_equal_right:('v2 -> 'v2 -> bool)
    -> ('k, 'v1, 'cmp) Map.t Incr.t
    -> ('k, 'v2, 'cmp) Map.t Incr.t
    -> f:(key:'k -> ('v1, 'v2) Map.Merge_element.t Incr.t -> 'v3 option Incr.t)
    -> ('k, 'v3, 'cmp) Map.t Incr.t

  val flatten : ('k, 'v Incr.t, 'cmp) Map.t -> ('k, 'v, 'cmp) Map.t Incr.t

  val join
    :  ?instrumentation:Instrumentation.t
    -> ('k, 'v Incr.t, 'cmp) Map.t Incr.t
    -> ('k, 'v, 'cmp) Map.t Incr.t

  val separate
    :  ?instrumentation:Instrumentation.t
    -> ('k, 'v, 'cmp) Map.t Incr.t
    -> data_equal:('v -> 'v -> bool)
    -> ('k, 'v Incr.t, 'cmp) Map.t Incr.t

  val keys
    :  ?instrumentation:Instrumentation.t
    -> ('k, 'v, 'c) Map.t Incr.t
    -> ('k, 'c) Set.t Incr.t

  val rank
    :  ?instrumentation:Instrumentation.t
    -> ('k, 'v, 'cmp) Base.Map.t Incr.t
    -> 'k Incr.t
    -> int option Incr.t

  val subrange
    :  ?instrumentation:Instrumentation.t
    -> ?data_equal:('v -> 'v -> bool)
    -> ('k, 'v, 'cmp) Map.t Incr.t
    -> ('k Maybe_bound.As_lower_bound.t * 'k Maybe_bound.As_upper_bound.t) option Incr.t
    -> ('k, 'v, 'cmp) Map.t Incr.t

  val subrange_by_rank
    :  ?instrumentation:Instrumentation.t
    -> ('k, 'v, 'cmp) Map.t Incr.t
    -> (int Maybe_bound.As_lower_bound.t * int Maybe_bound.As_upper_bound.t) Incr.t
    -> ('k, 'v, 'cmp) Map.t Incr.t

  val rekey
    :  ?instrumentation:Instrumentation.t
    -> ?data_equal:('v -> 'v -> bool)
    -> ('k1, 'v, 'cmp1) Map.t Incr.t
    -> comparator:('k2, 'cmp2) Comparator.Module.t
    -> f:(key:'k1 -> data:'v -> 'k2)
    -> ('k2, 'v, 'cmp2) Map.t Incr.t

  val index_byi
    :  ?instrumentation:Instrumentation.t
    -> ?data_equal:('v -> 'v -> bool)
    -> ('inner_key, 'v, 'inner_cmp) Map.t Incr.t
    -> comparator:('outer_key, 'outer_cmp) Comparator.Module.t
    -> index:(key:'inner_key -> data:'v -> 'outer_key option)
    -> ('outer_key, ('inner_key, 'v, 'inner_cmp) Map.t, 'outer_cmp) Map.t Incr.t

  val index_by
    :  ?instrumentation:Instrumentation.t
    -> ?data_equal:('v -> 'v -> bool)
    -> ('inner_key, 'v, 'inner_cmp) Map.t Incr.t
    -> comparator:('outer_key, 'outer_cmp) Comparator.Module.t
    -> index:('v -> 'outer_key option)
    -> ('outer_key, ('inner_key, 'v, 'inner_cmp) Map.t, 'outer_cmp) Map.t Incr.t

  val unordered_fold_nested_maps
    :  ?instrumentation:Instrumentation.t
    -> ?data_equal:('v -> 'v -> bool)
    -> ?revert_to_init_when_empty:bool
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
    :  ?instrumentation:Instrumentation.t
    -> ?data_equal:('v -> 'v -> bool)
    -> ('k2, 'k2_cmp) Comparator.Module.t
    -> ('k1, ('k2, 'v, 'k2_cmp) Map.t, 'k1_cmp) Map.t Incr.t
    -> ('k2, ('k1, 'v, 'k1_cmp) Map.t, 'k2_cmp) Map.t Incr.t

  val collapse
    :  ?instrumentation:Instrumentation.t
    -> ?data_equal:('v -> 'v -> bool)
    -> ('outer_key, ('inner_key, 'v, 'inner_cmp) Map.t, 'outer_cmp) Map.t Incr.t
    -> comparator:('inner_key, 'inner_cmp) Comparator.Module.t
    -> ( 'outer_key * 'inner_key
         , 'v
         , ('outer_cmp, 'inner_cmp) Tuple2.comparator_witness )
         Map.t
         Incr.t

  val collapse_by
    :  ?instrumentation:Instrumentation.t
    -> ?data_equal:('v -> 'v -> bool)
    -> ('outer_key, ('inner_key, 'v, 'inner_cmp) Map.t, 'outer_cmp) Map.t Incr.t
    -> merge_keys:('outer_key -> 'inner_key -> 'combined_key)
    -> comparator:('combined_key, 'combined_cmp) Comparator.Module.t
    -> ('combined_key, 'v, 'combined_cmp) Map.t Incr.t

  val collapse_by_loosened_requirements
    :  ?instrumentation:Instrumentation.t
    -> ?data_equal:('v -> 'v -> bool)
    -> ('outer_key, ('inner_key, 'v, 'inner_cmp) Map.t, 'outer_cmp) Map.t Incr.t
    -> merge_keys:('outer_key -> 'inner_key -> 'combined_key)
    -> comparator:('combined_key, 'combined_cmp) Comparator.Module.t
    -> ('combined_key, 'v, 'combined_cmp) Map.t Incr.t

  val expand
    :  ?instrumentation:Instrumentation.t
    -> ?data_equal:('v -> 'v -> bool)
    -> ('outer_key * 'inner_key, 'v, 'tuple_cmp) Map.t Incr.t
    -> outer_comparator:('outer_key, 'outer_cmp) Comparator.Module.t
    -> inner_comparator:('inner_key, 'inner_cmp) Comparator.Module.t
    -> ('outer_key, ('inner_key, 'v, 'inner_cmp) Map.t, 'outer_cmp) Map.t Incr.t

  val counti
    :  ?instrumentation:Instrumentation.t
    -> ?data_equal:('v -> 'v -> bool)
    -> ('k, 'v, _) Map.t Incr.t
    -> f:(key:'k -> data:'v -> bool)
    -> int Incr.t

  val count
    :  ?instrumentation:Instrumentation.t
    -> ?data_equal:('v -> 'v -> bool)
    -> (_, 'v, _) Map.t Incr.t
    -> f:('v -> bool)
    -> int Incr.t

  val for_alli
    :  ?instrumentation:Instrumentation.t
    -> ?data_equal:('v -> 'v -> bool)
    -> ('k, 'v, _) Map.t Incr.t
    -> f:(key:'k -> data:'v -> bool)
    -> bool Incr.t

  val for_all
    :  ?instrumentation:Instrumentation.t
    -> ?data_equal:('v -> 'v -> bool)
    -> (_, 'v, _) Map.t Incr.t
    -> f:('v -> bool)
    -> bool Incr.t

  val existsi
    :  ?instrumentation:Instrumentation.t
    -> ?data_equal:('v -> 'v -> bool)
    -> ('k, 'v, _) Map.t Incr.t
    -> f:(key:'k -> data:'v -> bool)
    -> bool Incr.t

  val exists
    :  ?instrumentation:Instrumentation.t
    -> ?data_equal:('v -> 'v -> bool)
    -> (_, 'v, _) Map.t Incr.t
    -> f:('v -> bool)
    -> bool Incr.t

  val sum
    :  ?instrumentation:Instrumentation.t
    -> ?data_equal:('v -> 'v -> bool)
    -> (_, 'v, _) Map.t Incr.t
    -> (module Abstract_algebra.Commutative_group.Without_sexp with type t = 'u)
    -> f:('v -> 'u)
    -> 'u Incr.t

  val observe_changes_exn
    :  ?data_equal:('v -> 'v -> bool)
    -> ('k, 'v, 'cmp) Map.t Incr.t
    -> f:(('k, 'v) Map.Symmetric_diff_element.t -> unit)
    -> unit

  val cartesian_product
    :  ?instrumentation:Instrumentation.t
    -> ?data_equal_left:('v1 -> 'v1 -> bool)
    -> ?data_equal_right:('v2 -> 'v2 -> bool)
    -> ('k1, 'v1, 'cmp1) Map.t Incr.t
    -> ('k2, 'v2, 'cmp2) Map.t Incr.t
    -> ('k1 * 'k2, 'v1 * 'v2, ('cmp1, 'cmp2) Tuple2.comparator_witness) Map.t Incr.t

  module Lookup : sig
    type ('k, 'v, 'cmp) t

    val create
      :  ?instrumentation:Instrumentation.t
      -> ?data_equal:('v -> 'v -> bool)
      -> ('k, 'cmp) Comparator.Module.t
      -> ('k, 'v, 'cmp) Map.t Incr.t
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
end

module type Incr_map = sig @@ portable
  (** Functions for using maps efficiently within Incremental. The goal of the algorithms
      here is to do work on the output of the computation proportional to the amount of
      work done on the input. i.e., [k] modifications to the input map for some
      computation will result in [k] modifications to the output map. The changes to the
      input map are typically computed using [Map.symmetric_diff].

      Unless stated otherwise, the non-incremental semantics of these functions (i.e..,
      ignoring performance) is the same as the corresponding function in Core's [Map]
      module. *)

  module Instrumentation = Instrumentation

  val of_set
    :  ?instrumentation:Instrumentation.t
    -> (('k, 'cmp) Set.t, 'w) Incremental.t
    -> (('k, unit, 'cmp) Map.t, 'w) Incremental.t

  val filter_mapi
    :  ?instrumentation:Instrumentation.t
    -> ?data_equal:('v1 -> 'v1 -> bool)
    -> (('k, 'v1, 'cmp) Map.t, 'w) Incremental.t
    -> f:(key:'k -> data:'v1 -> 'v2 option)
    -> (('k, 'v2, 'cmp) Map.t, 'w) Incremental.t

  val mapi
    :  ?instrumentation:Instrumentation.t
    -> ?data_equal:('v1 -> 'v1 -> bool)
    -> (('k, 'v1, 'cmp) Map.t, 'w) Incremental.t
    -> f:(key:'k -> data:'v1 -> 'v2)
    -> (('k, 'v2, 'cmp) Map.t, 'w) Incremental.t

  val filter_map
    :  ?instrumentation:Instrumentation.t
    -> ?data_equal:('v1 -> 'v1 -> bool)
    -> (('k, 'v1, 'cmp) Map.t, 'w) Incremental.t
    -> f:('v1 -> 'v2 option)
    -> (('k, 'v2, 'cmp) Map.t, 'w) Incremental.t

  val map
    :  ?instrumentation:Instrumentation.t
    -> ?data_equal:('v1 -> 'v1 -> bool)
    -> (('k, 'v1, 'cmp) Map.t, 'w) Incremental.t
    -> f:('v1 -> 'v2)
    -> (('k, 'v2, 'cmp) Map.t, 'w) Incremental.t

  val filter_mapi'
    :  ?instrumentation:Instrumentation.t
    -> ?cutoff:'v1 Incremental.Cutoff.t
    -> ?data_equal:('v1 -> 'v1 -> bool)
    -> (('k, 'v1, 'cmp) Map.t, 'w) Incremental.t
    -> f:(key:'k -> data:('v1, 'w) Incremental.t -> ('v2 option, 'w) Incremental.t)
    -> (('k, 'v2, 'cmp) Map.t, 'w) Incremental.t

  val mapi'
    :  ?instrumentation:Instrumentation.t
    -> ?cutoff:'v1 Incremental.Cutoff.t
    -> ?data_equal:('v1 -> 'v1 -> bool)
    -> (('k, 'v1, 'cmp) Map.t, 'w) Incremental.t
    -> f:(key:'k -> data:('v1, 'w) Incremental.t -> ('v2, 'w) Incremental.t)
    -> (('k, 'v2, 'cmp) Map.t, 'w) Incremental.t

  val filter_map'
    :  ?instrumentation:Instrumentation.t
    -> ?cutoff:'v1 Incremental.Cutoff.t
    -> ?data_equal:('v1 -> 'v1 -> bool)
    -> (('k, 'v1, 'cmp) Map.t, 'w) Incremental.t
    -> f:(('v1, 'w) Incremental.t -> ('v2 option, 'w) Incremental.t)
    -> (('k, 'v2, 'cmp) Map.t, 'w) Incremental.t

  val map'
    :  ?instrumentation:Instrumentation.t
    -> ?cutoff:'v1 Incremental.Cutoff.t
    -> ?data_equal:('v1 -> 'v1 -> bool)
    -> (('k, 'v1, 'cmp) Map.t, 'w) Incremental.t
    -> f:(('v1, 'w) Incremental.t -> ('v2, 'w) Incremental.t)
    -> (('k, 'v2, 'cmp) Map.t, 'w) Incremental.t

  val partition_mapi
    :  ?instrumentation:Instrumentation.t
    -> ?data_equal:('v1 -> 'v1 -> bool)
    -> (('k, 'v1, 'cmp) Map.t, 'w) Incremental.t
    -> f:(key:'k -> data:'v1 -> ('v2, 'v3) Either.t)
    -> (('k, 'v2, 'cmp) Map.t * ('k, 'v3, 'cmp) Map.t, 'w) Incremental.t

  val partition_mapi'
    :  ?instrumentation:Instrumentation.t
    -> ?cutoff:'v1 Incremental.Cutoff.t
    -> ?data_equal:('v1 -> 'v1 -> bool)
    -> (('k, 'v1, 'cmp) Map.t, 'w) Incremental.t
    -> f:
         (key:'k
          -> data:('v1, 'w) Incremental.t
          -> (('v2, 'v3) Either.t, 'w) Incremental.t)
    -> (('k, 'v2, 'cmp) Map.t * ('k, 'v3, 'cmp) Map.t, 'w) Incremental.t

  (** [unordered_fold i ~init ~add ~remove] constructs a more incremental version of:

      {[
        let%map m = i in
        Map.fold m ~init ~f:add
      ]}

      assuming that [remove] is the inverse of [add], and that the operations for
      different keys can be performed in any order. Note that [data_equal] defaults to
      [phys_equal], but a more precise equality can be provided instead.

      When the data for a key updates, by default [remove] is called on the old data and
      then [add] is called on the new data. [update] provides an alternative single
      function to call each time a key's data updates, and can be used to improve
      efficiency.

      For the initial computation, by default [add] is called on all the elements in the
      map. As this can be inefficient, [specialized_initial] can be provided to perform
      the computation in a more effective way.

      If [revert_to_init_when_empty] is true, then if the input map transitions from being
      full to empty, then instead of calling [remove] on every kv-pair, it will instead
      just set the output to whatever you've passed as [init]. The default value of
      [revert_to_init_when_empty] is [false], so this optimization does not apply
      automatically.

      [finalize] defaults to [Fn.id] is called immediately before the accumulator value is
      stored and returned during stabilization. You can use it to e.g. process the fold
      operations in a different order. *)
  val unordered_fold
    :  ?instrumentation:Instrumentation.t
    -> ?data_equal:('v -> 'v -> bool)
    -> ?update:(key:'k -> old_data:'v -> new_data:'v -> 'acc -> 'acc)
    -> ?specialized_initial:(init:'acc -> ('k, 'v, 'cmp) Map.t -> 'acc)
    -> ?finalize:('acc -> 'acc)
    -> ?revert_to_init_when_empty:bool
    -> (('k, 'v, 'cmp) Map.t, 'w) Incremental.t
    -> init:'acc
    -> add:(key:'k -> data:'v -> 'acc -> 'acc)
    -> remove:(key:'k -> data:'v -> 'acc -> 'acc)
    -> ('acc, 'w) Incremental.t

  (** [unordered_fold_with_extra] is similar to [unordered_fold], but it also depends on
      another arbitrary incremental value which can be factored into the folding
      computation. *)
  val unordered_fold_with_extra
    :  ?instrumentation:Instrumentation.t
    -> ?data_equal:('v -> 'v -> bool)
    -> ?extra_equal:('extra -> 'extra -> bool)
    -> ?update:(key:'k -> old_data:'v -> new_data:'v -> 'acc -> 'extra -> 'acc)
    -> ?specialized_initial:(init:'acc -> ('k, 'v, 'e) Map.t -> 'extra -> 'acc)
    -> ?finalize:('acc -> 'acc)
    -> ?revert_to_init_when_empty:bool
    -> (('k, 'v, 'e) Map.t, 'w) Incremental.t
    -> ('extra, 'w) Incremental.t
    -> init:'acc
    -> add:(key:'k -> data:'v -> 'acc -> 'extra -> 'acc)
    -> remove:(key:'k -> data:'v -> 'acc -> 'extra -> 'acc)
    -> extra_changed:
         (old_extra:'extra
          -> new_extra:'extra
          -> input:('k, 'v, 'e) Map.t
          -> 'acc
          -> 'acc)
    -> ('acc, 'w) Incremental.t

  (** [cutoff] applies a cutoff to values in the map as they pass through the function. It
      has the same behavior as calling [Incr_map.map'] with an [Incr.set_cutoff] inside,
      but with considerably better performance and memory usage. *)
  val cutoff
    :  ?instrumentation:Instrumentation.t
    -> (('k, 'v, 'cmp) Map.t, 'w) Incremental.t
    -> cutoff:'v Incremental.Cutoff.t
    -> (('k, 'v, 'cmp) Map.t, 'w) Incremental.t

  (** Given an input map and a function mapping a kv-pair to a new value, [mapi_count]
      will compute a multi-set keyed on that new value.

      Any value that would otherwise have a count of "0" is instead removed from the map.

      It is assumed that [f] is quite fast as the function will be called more often than
      strictly necessary, but it does this in order to avoid allocating an extra map. If
      [f] is very slow and you don't mind the extra allocations, use [Incr_map.index_byi]
      composed with [Incr_map.map ~f:Map.length] *)
  val mapi_count
    :  ?instrumentation:Instrumentation.t
    -> ?data_equal:('v -> 'v -> bool)
    -> (('k1, 'v, 'cmp1) Map.t, 'w) Incremental.t
    -> comparator:('k2, 'cmp2) Comparator.Module.t
    -> f:(key:'k1 -> data:'v -> 'k2)
    -> (('k2, int, 'cmp2) Map.t, 'w) Incremental.t

  (** The same as [mapi_count] but the [f] function only gets to see the data instead of
      both the key and the data. *)
  val map_count
    :  ?instrumentation:Instrumentation.t
    -> ?data_equal:('v -> 'v -> bool)
    -> (('k1, 'v, 'cmp1) Map.t, 'w) Incremental.t
    -> comparator:('k2, 'cmp2) Comparator.Module.t
    -> f:('v -> 'k2)
    -> (('k2, int, 'cmp2) Map.t, 'w) Incremental.t

  (** Computes the smallest [r] where [r] is computed for each kv-pair in the input map. *)
  val mapi_min
    :  ?instrumentation:Instrumentation.t
    -> ?data_equal:('v -> 'v -> bool)
    -> (('k, 'v, _) Map.t, 'w) Incremental.t
    -> comparator:('r, _) Comparator.Module.t
    -> f:(key:'k -> data:'v -> 'r)
    -> ('r option, 'w) Incremental.t

  (** Computes the largest [r] where [r] is computed for each kv-pair in the input map. *)
  val mapi_max
    :  ?instrumentation:Instrumentation.t
    -> ?data_equal:('v -> 'v -> bool)
    -> (('k, 'v, _) Map.t, 'w) Incremental.t
    -> comparator:('r, _) Comparator.Module.t
    -> f:(key:'k -> data:'v -> 'r)
    -> ('r option, 'w) Incremental.t

  (** Computes the smallest [r] where [r] is computed for each kv-pair in the input map. *)
  val map_min
    :  ?instrumentation:Instrumentation.t
    -> ?data_equal:('v -> 'v -> bool)
    -> (('k, 'v, _) Map.t, 'w) Incremental.t
    -> comparator:('r, _) Comparator.Module.t
    -> f:('v -> 'r)
    -> ('r option, 'w) Incremental.t

  (** Computes the largest [r] where [r] is computed for each kv-pair in the input map. *)
  val map_max
    :  ?instrumentation:Instrumentation.t
    -> ?data_equal:('v -> 'v -> bool)
    -> (('k, 'v, _) Map.t, 'w) Incremental.t
    -> comparator:('r, _) Comparator.Module.t
    -> f:('v -> 'r)
    -> ('r option, 'w) Incremental.t

  (** Computes the smallest data value from the input map. *)
  val min_value
    :  ?instrumentation:Instrumentation.t
    -> ?data_equal:('v -> 'v -> bool)
    -> (('k, 'v, _) Map.t, 'w) Incremental.t
    -> comparator:('v, _) Comparator.Module.t
    -> ('v option, 'w) Incremental.t

  (** Computes the largest data value from the input map. *)
  val max_value
    :  ?instrumentation:Instrumentation.t
    -> ?data_equal:('v -> 'v -> bool)
    -> (('k, 'v, _) Map.t, 'w) Incremental.t
    -> comparator:('v, _) Comparator.Module.t
    -> ('v option, 'w) Incremental.t

  (** Computes [min * max] where the value is computed for each kv-pair in the input map *)
  val mapi_bounds
    :  ?instrumentation:Instrumentation.t
    -> ?data_equal:('v -> 'v -> bool)
    -> (('k, 'v, _) Map.t, 'w) Incremental.t
    -> comparator:('r, _) Comparator.Module.t
    -> f:(key:'k -> data:'v -> 'r)
    -> (('r * 'r) option, 'w) Incremental.t

  (** Computes [min * max] where the value is computed for each kv-pair in the input map *)
  val map_bounds
    :  ?instrumentation:Instrumentation.t
    -> ?data_equal:('v -> 'v -> bool)
    -> (('k, 'v, _) Map.t, 'w) Incremental.t
    -> comparator:('r, _) Comparator.Module.t
    -> f:('v -> 'r)
    -> (('r * 'r) option, 'w) Incremental.t

  (** Computes the smallest and largest data value from the input map. *)
  val value_bounds
    :  ?instrumentation:Instrumentation.t
    -> ?data_equal:('v -> 'v -> bool)
    -> (('k, 'v, _) Map.t, 'w) Incremental.t
    -> comparator:('v, _) Comparator.Module.t
    -> (('v * 'v) option, 'w) Incremental.t

  (** Like [merge] in [Base.Map.merge]. Note that [f] is called at most once per key in
      any given stabilization. *)
  val merge
    :  ?instrumentation:Instrumentation.t
    -> ?data_equal_left:('v1 -> 'v1 -> bool)
    -> ?data_equal_right:('v2 -> 'v2 -> bool)
    -> (('k, 'v1, 'cmp) Map.t, 'w) Incremental.t
    -> (('k, 'v2, 'cmp) Map.t, 'w) Incremental.t
    -> f:(key:'k -> ('v1, 'v2) Map.Merge_element.t -> 'v3 option)
    -> (('k, 'v3, 'cmp) Map.t, 'w) Incremental.t

  (** [merge_both_same] is like [merge], but optimized for the case where you only care
      about the case where both maps contain a particular key. *)
  val merge_both_some
    :  ?instrumentation:Instrumentation.t
    -> ?data_equal_left:('v1 -> 'v1 -> bool)
    -> ?data_equal_right:('v2 -> 'v2 -> bool)
    -> ?out_equal:('v3 -> 'v3 -> bool)
    -> (('k, 'v1, 'cmp) Map.t, 'w) Incremental.t
    -> (('k, 'v2, 'cmp) Map.t, 'w) Incremental.t
    -> f:(key:'k -> 'v1 -> 'v2 -> 'v3)
    -> (('k, 'v3, 'cmp) Map.t, 'w) Incremental.t

  (** [merge_disjoint] merges two maps that _must_ not share any keys on a given
      stabilization. If this invariant is not upheld by the caller, incremental may crash,
      or the output map may contain incorrect results. *)
  val merge_disjoint
    :  ?instrumentation:Instrumentation.t
    -> ?data_equal:('v -> 'v -> bool)
    -> (('k, 'v, 'cmp) Map.t, 'w) Incremental.t
    -> (('k, 'v, 'cmp) Map.t, 'w) Incremental.t
    -> (('k, 'v, 'cmp) Map.t, 'w) Incremental.t

  (** Like [merge], but operating using incremental nodes. This is a good use case for
      [ppx_pattern_bind]. *)
  val merge'
    :  ?instrumentation:Instrumentation.t
    -> ?cutoff:('v1, 'v2) Map.Merge_element.t Incremental.Cutoff.t
    -> ?data_equal_left:('v1 -> 'v1 -> bool)
    -> ?data_equal_right:('v2 -> 'v2 -> bool)
    -> (('k, 'v1, 'cmp) Map.t, 'w) Incremental.t
    -> (('k, 'v2, 'cmp) Map.t, 'w) Incremental.t
    -> f:
         (key:'k
          -> (('v1, 'v2) Map.Merge_element.t, 'w) Incremental.t
          -> ('v3 option, 'w) Incremental.t)
    -> (('k, 'v3, 'cmp) Map.t, 'w) Incremental.t

  val unzip
    :  ?instrumentation:Instrumentation.t
    -> ?left_result_equal:('a -> 'a -> bool)
    -> ?right_result_equal:('b -> 'b -> bool)
    -> (('k, 'a * 'b, 'cmp) Map.t, 'w) Incremental.t
    -> (('k, 'a, 'cmp) Map.t, 'w) Incremental.t * (('k, 'b, 'cmp) Map.t, 'w) Incremental.t

  (** [unzip_mapi] is similar to [List.unzip], but for incremental maps. Note that [f] may
      be called multiple times on a single element. *)
  val unzip_mapi
    :  ?instrumentation:Instrumentation.t
    -> ?data_equal:('v -> 'v -> bool)
    -> ?left_result_equal:('v1 -> 'v1 -> bool)
    -> ?right_result_equal:('v2 -> 'v2 -> bool)
    -> (('k, 'v, 'cmp) Map.t, 'w) Incremental.t
    -> f:(key:'k -> data:'v -> 'v1 * 'v2)
    -> (('k, 'v1, 'cmp) Map.t, 'w) Incremental.t
       * (('k, 'v2, 'cmp) Map.t, 'w) Incremental.t

  (** [unzip_mapi'] is like [unzip_mapi], but allows you to define the mapping from the
      input map's elements to the output maps' elements incrementally.

      The naive implementation (see below) produces worse Incremental graphs.

      {[
        let temp =
          Incr_map.mapi' input ~f:(fun ~key ~data ->
            f ~key ~data |> Tuple2.uncurry Incr.both)
        in
        let left = Incr_map.map temp ~f:Tuple2.get1 in
        let right = Incr_map.map temp ~f:Tuple2.get2 in
        left, right
      ]} *)
  val unzip_mapi'
    :  ?instrumentation:Instrumentation.t
    -> ?cutoff:'v Incremental.Cutoff.t
    -> ?data_equal:('v -> 'v -> bool)
    -> (('k, 'v, 'cmp) Map.t, 'w) Incremental.t
    -> f:
         (key:'k
          -> data:('v, 'w) Incremental.t
          -> ('v1, 'w) Incremental.t * ('v2, 'w) Incremental.t)
    -> (('k, 'v1, 'cmp) Map.t, 'w) Incremental.t
       * (('k, 'v2, 'cmp) Map.t, 'w) Incremental.t

  (** [unzip3_mapi'] is like [unzip_mapi'], but allows you to return a 3-tuple instead of
      a 2-tuple of incremental nodes

      The naive implementation (see below) is behaviorally identical, but produces worse
      Incremental graphs.

      {[
        let temp =
          Incr_map.mapi' input ~f:(fun ~key ~data ->
            let a, b, c = f ~key ~data in
            let%map a and b and c = a, b, c)
        in
        let left = Incr_map.map temp ~f:Tuple3.get1 in
        let middle = Incr_map.map temp ~f:Tuple3.get2 in
        let right = Incr_map.map temp ~f:Tuple3.get3 in
        left, middle, right
      ]} *)
  val unzip3_mapi'
    :  ?instrumentation:Instrumentation.t
    -> ?cutoff:'v Incremental.Cutoff.t
    -> ?data_equal:('v -> 'v -> bool)
    -> (('k, 'v, 'cmp) Map.t, 'w) Incremental.t
    -> f:
         (key:'k
          -> data:('v, 'w) Incremental.t
          -> ('v1, 'w) Incremental.t * ('v2, 'w) Incremental.t * ('v3, 'w) Incremental.t)
    -> (('k, 'v1, 'cmp) Map.t, 'w) Incremental.t
       * (('k, 'v2, 'cmp) Map.t, 'w) Incremental.t
       * (('k, 'v3, 'cmp) Map.t, 'w) Incremental.t

  (** This is the "easy" version of [join] *)
  val flatten
    :  'w Incremental.State.t
    -> ('k, ('v, 'w) Incremental.t, 'cmp) Map.t
    -> (('k, 'v, 'cmp) Map.t, 'w) Incremental.t

  (** The non-incremental semantics of this function is the identity function. Its purpose
      is to collapse the extra level of incrementality at the level of the data of the
      map. *)
  val join
    :  ?instrumentation:Instrumentation.t
    -> (('k, ('v, 'w) Incremental.t, 'cmp) Map.t, 'w) Incremental.t
    -> (('k, 'v, 'cmp) Map.t, 'w) Incremental.t

  val separate
    :  ?instrumentation:Instrumentation.t
    -> (('k, 'v, 'cmp) Map.t, 'w) Incremental.t
    -> data_equal:('v -> 'v -> bool)
    -> (('k, ('v, 'w) Incremental.t, 'cmp) Map.t, 'w) Incremental.t

  val keys
    :  ?instrumentation:Instrumentation.t
    -> (('k, 'v, 'c) Map.t, 'w) Incremental.t
    -> (('k, 'c) Set.t, 'w) Incremental.t

  (** Computes the [rank] of a key (given incrementally) inside of a map (also
      incremental). The traditional [Map.rank] function is O(n), and this incremental rank
      function has the following performance characteristics:

      definitions: n : the size of the map r : the time to compute [Map.symmetric_diff]
      between the two maps k : the change in rank of the key between two stabilizations

      note that [r] and [k] are _much_ smaller than [n] for most practical purposes

      - O(log n) when the key is not in the map. This takes precedence over other every
        other scenario.
      - O(n) on the initial stabilization
      - O(n) when the key transitions from not being in the map to being in the map
      - O(log n + r) when the map changes
      - O(log n + k) when the key changes
      - O(log n + r + k) when both key and map change *)
  val rank
    :  ?instrumentation:Instrumentation.t
    -> (('k, 'v, 'cmp) Base.Map.t, 'state_witness) Incremental.t
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
    :  ?instrumentation:Instrumentation.t
    -> ?data_equal:('v -> 'v -> bool)
    -> (('k, 'v, 'cmp) Map.t, 'w) Incremental.t
    -> ( ('k Maybe_bound.As_lower_bound.t * 'k Maybe_bound.As_upper_bound.t) option
         , 'w )
         Incremental.t
    -> (('k, 'v, 'cmp) Map.t, 'w) Incremental.t

  (** {v
 [subrange_by_rank map (s, e)] constructs an incremental submap that includes (e-s+1)
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
      v} *)
  val subrange_by_rank
    :  ?instrumentation:Instrumentation.t
    -> (('k, 'v, 'cmp) Map.t, 'w) Incremental.t
    -> ( int Maybe_bound.As_lower_bound.t * int Maybe_bound.As_upper_bound.t
         , 'w )
         Incremental.t
    -> (('k, 'v, 'cmp) Map.t, 'w) Incremental.t

  (** [rekey] transforms a map by modifying the type of the key. The user is responsible
      for ensuring that [f] doesn't return the same output key for multiple input keys.

      This function assumes [f] is cheap to compute and accordingly may call it multiple
      times. *)
  val rekey
    :  ?instrumentation:Instrumentation.t
    -> ?data_equal:('v -> 'v -> bool)
    -> (('k1, 'v, 'cmp1) Map.t, 'w) Incremental.t
    -> comparator:('k2, 'cmp2) Comparator.Module.t
    -> f:(key:'k1 -> data:'v -> 'k2)
    -> (('k2, 'v, 'cmp2) Map.t, 'w) Incremental.t

  (** [index_byi map ~comparator ~index] constructs an incremental map-of-maps where each
      key-data pair of the input map is present in one (or none) of the inner maps.
      [index] specifies the outer map key under which each original key-data pair is
      found.

      All of the resulting inner maps are guaranteed to be non-empty; if the inner map
      would otherwise be empty, then the key for that map is instead removed from the
      outer map.

      An all-at-once version of [index_by] would look like:

      {[
        let index_byi map ~comparator ~index =
          Map.to_alist map
          |> List.filter_map ~f:(fun (key, data) ->
            match index ~key ~data with
            | None -> None
            | Some index -> Some (index, (key, data)))
          |> Map.of_alist_multi comparator
          |> Map.map ~f:(Map.of_alist_exn (Map.comparator_s map))
        ;;
      ]} *)
  val index_byi
    :  ?instrumentation:Instrumentation.t
    -> ?data_equal:('v -> 'v -> bool)
    -> (('inner_key, 'v, 'inner_cmp) Map.t, 'w) Incremental.t
    -> comparator:('outer_key, 'outer_cmp) Comparator.Module.t
    -> index:(key:'inner_key -> data:'v -> 'outer_key option)
    -> ( ('outer_key, ('inner_key, 'v, 'inner_cmp) Map.t, 'outer_cmp) Map.t
         , 'w )
         Incremental.t

  (** [index_by map ~comparator ~index] is like [index_byi map ~comparator ~index], but
      the [index] function does not take the inner map's [key]. *)
  val index_by
    :  ?instrumentation:Instrumentation.t
    -> ?data_equal:('v -> 'v -> bool)
    -> (('inner_key, 'v, 'inner_cmp) Map.t, 'w) Incremental.t
    -> comparator:('outer_key, 'outer_cmp) Comparator.Module.t
    -> index:('v -> 'outer_key option)
    -> ( ('outer_key, ('inner_key, 'v, 'inner_cmp) Map.t, 'outer_cmp) Map.t
         , 'w )
         Incremental.t

  val unordered_fold_nested_maps
    :  ?instrumentation:Instrumentation.t
    -> ?data_equal:('v -> 'v -> bool)
    -> ?revert_to_init_when_empty:bool
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

  (** [transpose] flips the order of a doubly nested incremental map.

      All inner map instances will have at least one element. *)
  val transpose
    :  ?instrumentation:Instrumentation.t
    -> ?data_equal:('v -> 'v -> bool)
    -> ('k2, 'k2_cmp) Comparator.Module.t
    -> (('k1, ('k2, 'v, 'k2_cmp) Map.t, 'k1_cmp) Map.t, 'w) Incremental.t
    -> (('k2, ('k1, 'v, 'k1_cmp) Map.t, 'k2_cmp) Map.t, 'w) Incremental.t

  val collapse
    :  ?instrumentation:Instrumentation.t
    -> ?data_equal:('v -> 'v -> bool)
    -> ( ('outer_key, ('inner_key, 'v, 'inner_cmp) Map.t, 'outer_cmp) Map.t
         , 'w )
         Incremental.t
    -> comparator:('inner_key, 'inner_cmp) Comparator.Module.t
    -> ( ( 'outer_key * 'inner_key
           , 'v
           , ('outer_cmp, 'inner_cmp) Tuple2.comparator_witness )
           Map.t
         , 'w )
         Incremental.t

  (** {v
 [collapse_by] is similar to [collapse], but it allows the user to
      choose how to combine the two keys from the outer and inner maps.
      This does mean that it's the responsibility of the implementor of the
      [merge_keys] function to uphold this invariant:

      > a merged-key being equal to another merged-key implies that the
      > outer-keys and inner-keys which were used to build the merged keys also
      > compare to be equal to one another

      The [~comparator] argument the first-class module of the output key, it
      usually looks like this:
      [ ~comparator:(module Combined_key) ]
      but make sure that the module implements the [Comparator.S] signature.
      v} *)
  val collapse_by
    :  ?instrumentation:Instrumentation.t
    -> ?data_equal:('v -> 'v -> bool)
    -> ( ('outer_key, ('inner_key, 'v, 'inner_cmp) Map.t, 'outer_cmp) Map.t
         , 'w )
         Incremental.t
    -> merge_keys:('outer_key -> 'inner_key -> 'combined_key)
    -> comparator:('combined_key, 'combined_cmp) Comparator.Module.t
    -> (('combined_key, 'v, 'combined_cmp) Map.t, 'w) Incremental.t

  (** {v
 Just like [collapse_by] but instead of the user-upheld invariant being

      > a merged-key being equal to another merged-key implies that the
      > outer-keys and inner-keys which were used to build the merged keys also
      > compare to be equal to one another

      the invariant is instead

      > the input map must contain no duplicate combined keys at any point in time

      This invariant is easier to uphold, but is slightly slower, as
      [collapse_by_loosened_requirements] needs to do some extra bookkeeping.
      In addition, this function makes no attempt at enforcing this invariant;
      if the input does contain duplicate combined keys, the resulting map may
      contain incorrect data.
      v} *)
  val collapse_by_loosened_requirements
    :  ?instrumentation:Instrumentation.t
    -> ?data_equal:('v -> 'v -> bool)
    -> ( ('outer_key, ('inner_key, 'v, 'inner_cmp) Map.t, 'outer_cmp) Map.t
         , 'w )
         Incremental.t
    -> merge_keys:('outer_key -> 'inner_key -> 'combined_key)
    -> comparator:('combined_key, 'combined_cmp) Comparator.Module.t
    -> (('combined_key, 'v, 'combined_cmp) Map.t, 'w) Incremental.t

  (** Convert a map with tuples for keys into a nested map. This operation is roughly the
      inverse of [collapse], though if there are outer keys in the uncollapsed map that
      correspond to empty inner maps, the outer keys will be dropped from the expanded
      map. *)
  val expand
    :  ?instrumentation:Instrumentation.t
    -> ?data_equal:('v -> 'v -> bool)
    -> (('outer_key * 'inner_key, 'v, 'tuple_cmp) Map.t, 'w) Incremental.t
    -> outer_comparator:('outer_key, 'outer_cmp) Comparator.Module.t
    -> inner_comparator:('inner_key, 'inner_cmp) Comparator.Module.t
    -> ( ('outer_key, ('inner_key, 'v, 'inner_cmp) Map.t, 'outer_cmp) Map.t
         , 'w )
         Incremental.t

  val counti
    :  ?instrumentation:Instrumentation.t
    -> ?data_equal:('v -> 'v -> bool)
    -> (('k, 'v, _) Map.t, 'w) Incremental.t
    -> f:(key:'k -> data:'v -> bool)
    -> (int, 'w) Incremental.t

  val count
    :  ?instrumentation:Instrumentation.t
    -> ?data_equal:('v -> 'v -> bool)
    -> ((_, 'v, _) Map.t, 'w) Incremental.t
    -> f:('v -> bool)
    -> (int, 'w) Incremental.t

  val for_alli
    :  ?instrumentation:Instrumentation.t
    -> ?data_equal:('v -> 'v -> bool)
    -> (('k, 'v, _) Map.t, 'w) Incremental.t
    -> f:(key:'k -> data:'v -> bool)
    -> (bool, 'w) Incremental.t

  val for_all
    :  ?instrumentation:Instrumentation.t
    -> ?data_equal:('v -> 'v -> bool)
    -> ((_, 'v, _) Map.t, 'w) Incremental.t
    -> f:('v -> bool)
    -> (bool, 'w) Incremental.t

  val existsi
    :  ?instrumentation:Instrumentation.t
    -> ?data_equal:('v -> 'v -> bool)
    -> (('k, 'v, _) Map.t, 'w) Incremental.t
    -> f:(key:'k -> data:'v -> bool)
    -> (bool, 'w) Incremental.t

  val exists
    :  ?instrumentation:Instrumentation.t
    -> ?data_equal:('v -> 'v -> bool)
    -> ((_, 'v, _) Map.t, 'w) Incremental.t
    -> f:('v -> bool)
    -> (bool, 'w) Incremental.t

  (** Incrementally compute the sum of all of the values in the map.

      Beware of float's negative infinities. They aren't commutative and will misbehave
      here. *)
  val sum
    :  ?instrumentation:Instrumentation.t
    -> ?data_equal:('v -> 'v -> bool)
    -> ((_, 'v, _) Map.t, 'w) Incremental.t
    -> (module Abstract_algebra.Commutative_group.Without_sexp with type t = 'u)
    -> f:('v -> 'u)
    -> ('u, 'w) Incremental.t

  (** Observes changes to an incremental map. Every stabilize, this observer will compare
      the map from the previous stabilization and the current stabilization, calling [f]
      for every change that it detects.

      It is important to note that changes to the map _between_ stabilizations will not be
      processed, for example:

      {[
        let var = Incr.Var.create String.Map.empty in
        Incr_map.observe_changes_exn (Incr.Var.watch var) ~f:(...);

        Incr.stabilize ();

        Incr.Var.replace var ~f:(fun map -> Map.add_exn map "hi" 5);
        Incr.Var.replace var ~f:(fun map -> Map.set map "hi" 10);
        Incr.Var.replace var ~f:(fun map -> Map.remove map "hi");
        Incr.stabilize ();
      ]}

      won't result in any calls to [f], because the map contents didn't change from one
      stabilization to another.

      [observe_changes_exn] must only be called from the top-level incremental scope. In
      practice this means that it must not be inside of an incremental bind, or a call to
      [Incremental.Scope.within]. If not invoked at top-level, an exception will be
      raised, irreversibly destroying your incremental universe. *)
  val observe_changes_exn
    :  ?data_equal:('v -> 'v -> bool)
    -> (('k, 'v, 'cmp) Map.t, _) Incremental.t
    -> f:(('k, 'v) Map.Symmetric_diff_element.t -> unit)
    -> unit
    @@ (* not portable because it requires [Incremental.observe] to be portable *)
       nonportable

  (** [cartesian_product] provides a way to compute the cartesian product of two maps,
      performed incrementally. The number of keys in the resulting map is the product of
      the number keys of each map.

      Definitions: n1,n2 : size of maps (the max of the size of the map between two
      stabilizations) r1,r2 : time to call symmetric diff on each map between two
      stabilizations k1,k2 : size of the diff of each map as a result of symmetric diff
      between two stablizations

      The marginal stabilization runs in:
      [O(r1 + r2 + ((k1 * n2) + (k2 * n1)) * log (n1 * n2))] *)
  val cartesian_product
    :  ?instrumentation:Instrumentation.t
    -> ?data_equal_left:('v1 -> 'v1 -> bool)
    -> ?data_equal_right:('v2 -> 'v2 -> bool)
    -> (('k1, 'v1, 'cmp1) Map.t, 'w) Incremental.t
    -> (('k2, 'v2, 'cmp2) Map.t, 'w) Incremental.t
    -> ( ('k1 * 'k2, 'v1 * 'v2, ('cmp1, 'cmp2) Tuple2.comparator_witness) Map.t
         , 'w )
         Incremental.t

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
      is [O(k * log n)]. *)
  module Lookup : sig
    type ('k, 'v, 'cmp, 'w) t

    (** Create the lookup structure on an incremental map. *)
    val create
      :  ?instrumentation:Instrumentation.t
      -> ?data_equal:('v -> 'v -> bool)
      -> ('k, 'cmp) Comparator.Module.t
      -> (('k, 'v, 'cmp) Map.t, 'w) Incremental.t
      -> ('k, 'v, 'cmp, 'w) t

    (** Create a node which performs [Map.find] on the input map.

        [find (create incr_map) key] should be equivalent to
        [Incr.map ~f:(fun m -> Map.find m key) incr_map], but when you call [find] many
        times for a single [create] the nodes should update more efficiently in
        stabilisation when [incr_map] changes in a way which can be efficiently diffed.

        This will re-use existing nodes when it can, but will not always do so. *)
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
