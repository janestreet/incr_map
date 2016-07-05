
open! Core_kernel.Std

module Make (Incr: Incremental_kernel.Incremental_intf.S) : sig
  val filter_mapi
    :  ?data_equal:('v1 -> 'v1 -> bool)
    -> ('k, 'v1, 'cmp) Map.t Incr.t
    -> f:(key:'k -> data:'v1 -> 'v2 option)
    -> ('k, 'v2, 'cmp) Map.t Incr.t

  val filter_mapi'
    :  ?cutoff:'v1 Incr.Cutoff.t
    -> ?data_equal:('v1 -> 'v1 -> bool)
    -> ('k, 'v1, 'cmp) Map.t Incr.t
    -> f:(key:'k -> data:'v1 Incr.t -> 'v2 option Incr.t)
    -> ('k, 'v2, 'cmp) Map.t Incr.t

  (** [unordered_fold i ~init ~f ~f_inverse] constructs a more incremental version of:

      {[
        let%map m = i in
        Map.fold m ~init ~f
      ]}

      assuming that [f_inverse] is the inverse of [f], and that the operations for
      different keys can be performed in any order. Note that [data_equal] defaults
      to [phys_equal], but a more precise equality can be provided instead.  *)
  val unordered_fold
    :  ?data_equal:('v -> 'v -> bool)
    -> ('k, 'v, 'cmp) Map.t Incr.t
    -> init:'acc
    -> f:(key:'k -> data:'v -> 'acc -> 'acc)
    -> f_inverse:(key:'k -> data:'v -> 'acc -> 'acc)
    -> 'acc Incr.t

  val merge
    :  ('k, 'v1, 'cmp) Map.t Incr.t
    -> ('k, 'v2, 'cmp) Map.t Incr.t
    -> f:(key:'k
          -> [ `Left of 'v1
             | `Right of 'v2
             | `Both of ('v1 * 'v2) ]
          -> 'v3 option)
    -> ( 'k, 'v3, 'cmp) Map.t Incr.t

  (* This is the "easy" version of [map_join] *)
  val flatten : ('k, 'v Incr.t, 'cmp) Map.t -> ('k, 'v, 'cmp) Map.t Incr.t

  val join
    :  ('k, 'v Incr.t, 'cmp) Map.t Incr.t
    -> ('k, 'v, 'cmp) Map.t Incr.t
end
