open! Core
open! Import

(** When building a set incrementally, in an unordered way, it's necessary to guarantee
    that a single [Set.remove] call does not erase more than one [Set.add]. This data
    structure keeps counts of the adds and removes so that an element is only removed once
    the number of calls to [remove] equals the number of calls to [add].

    The main difference between this library and [Multi_set] is the existence of the O(1)
    [to_set]. *)

type ('ele, 'cmp) t

val empty : comparator:('ele, 'cmp) Comparator.t -> ('ele, 'cmp) t

(** Create a [t] where every element in [Set.t] is added once. *)
val of_set : ('ele, 'cmp) Set.t -> ('ele, 'cmp) t

(** Operates in O(1) *)
val to_set : ('ele, 'cmp) t -> ('ele, 'cmp) Set.t

val add : ('ele, 'cmp) t -> 'ele -> ('ele, 'cmp) t
val remove : ('ele, 'cmp) t -> 'ele -> ('ele, 'cmp) t
val is_empty : ('ele, 'cmp) t -> bool

(** [true] iff the invariants of the data structure hold. *)
val invariants : (_, _) t -> bool
