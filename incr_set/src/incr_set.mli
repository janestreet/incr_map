open! Core
open! Import

(** Operate on sets incrementally. The operations below make heavy use of
    [Set.symmetric_diff] to minimize the amount of work done per stabilization.

    In the discussion of computational complexity, the following conventions are used:
    - [s<i>'] denotes the size of the i-th set at the time of the last stabilization
    - [s<i>] denotes the size of the i-th sets at the time of the current stabilization
    - [d<i>] represents the time complexity of calling [Set.symmetric_diff] on [s<i>'] and
      [s<i>]
    - [k<i>] represents the number of removals/additions that need to be applied to
      convert [s<i>'] into [s<i>] *)

(** [union] is equivalent to [Set.union], but performed incrementally.

    The marginal stabilization should require work that is O(d1 + d2 + k1*log(s2) +
    k2*log(s1)) *)
val union
  :  (('a, 'cmp) Set.t, 'w) Incremental.t
  -> (('a, 'cmp) Set.t, 'w) Incremental.t
  -> (('a, 'cmp) Set.t, 'w) Incremental.t

(** [inter] is equivalent to [Set.inter], but performed incrmeentally.

    The marginal stabilization should require work that is O(d1 + d2 + k1*log(s2) +
    k2*log(s1)) *)
val inter
  :  (('a, 'cmp) Set.t, 'w) Incremental.t
  -> (('a, 'cmp) Set.t, 'w) Incremental.t
  -> (('a, 'cmp) Set.t, 'w) Incremental.t

(** [diff] is equivalent to [Set.diff], but performed incrementally.

    The marginal stabilization should require work that is O(d1 + d2 + k1*log(s2) +
    k2*log(s1)) *)
val diff
  :  (('a, 'cmp) Set.t, 'w) Incremental.t
  -> (('a, 'cmp) Set.t, 'w) Incremental.t
  -> (('a, 'cmp) Set.t, 'w) Incremental.t

(** [filter] is equivalent to [Set.filter], but performed incrementally.

    The marginal stabilization should require work that is O(d + k*(log(s)) + f) *)
val filter
  :  (('a, 'cmp) Set.t, 'w) Incremental.t
  -> f:('a -> bool)
  -> (('a, 'cmp) Set.t, 'w) Incremental.t

val unordered_fold
  :  (('a, 'cmp) Set.t, 'w) Incremental.t
  -> init:'acc
  -> add:('acc -> 'a -> 'acc)
  -> remove:('acc -> 'a -> 'acc)
  -> ('acc, 'w) Incremental.t

(** [cartesian_product] is equivalent to the cartesian product of both sets, performed
    incrementally.

    The marginal stabilization should require work that is O(d1 + d2 + (k1*max(s2',s2) +
    k2*max(s1',s1))*log(max(s1',s1)*max(s2',s2))) *)
val cartesian_product
  :  (('a1, 'cmp1) Set.t, 'w) Incremental.t
  -> (('a2, 'cmp2) Set.t, 'w) Incremental.t
  -> (('a1 * 'a2, ('cmp1, 'cmp2) Tuple2.comparator_witness) Set.t, 'w) Incremental.t

val union_map_data
  :  comparator:('a, 'cmp) Comparator.t
  -> ((_, ('a, 'cmp) Set.t, _) Map.t, 'w) Incremental.t
  -> (('a, 'cmp) Set.t, 'w) Incremental.t

(** Observes changes to an incremental set. Every stabilize, this observer will compare
    the set from the previous stabilization and the current stabilization, calling [f] for
    every change that it detects.

    It is important to note that changes to the set _between_ stabilizations will not be
    processed, for example:

    {[
      let var = Incr.Var.create String.Set.empty in
      Incr_set.observe_changes_exn (Incr.Var.watch var) ~f:(...);

      Incr.stabilize ();

      Incr.Var.replace var ~f:(fun set -> Set.add_exn set "hi");
      Incr.Var.replace var ~f:(fun set -> Set.remove set "hi");
      Incr.stabilize ();
    ]}

    won't result in any calls to [f], because the set contents didn't change from one
    stabilization to another.

    [observe_changes_exn] must only be called from the top-level incremental scope. In
    practice this means that it must not be inside of an incremental bind, or a call to
    [Incremental.Scope.within]. If not invoked at top-level, an exception will be raised,
    irreversibly destroying your incremental universe. *)
val observe_changes_exn
  :  (('a, 'cmp) Set.t, _) Incremental.t
  -> on_add:('a -> unit)
  -> on_remove:('a -> unit)
  -> unit
