open! Core_kernel
module Collate = Collate
module Collated = Collated
module Map_list = Map_list

module Compare : sig
  type ('k, 'v) t =
    | Unchanged
    | Reversed
    | Custom_by_value of { compare : 'v -> 'v -> int }
    | Custom_by_key_and_value of { compare : 'k * 'v -> 'k * 'v -> int }
    (** Partial orders are supported in Custom_by_*, i.e. returning 0 shouldn't cause
        issues. Rows will be then sorted by key. *)
  [@@deriving sexp_of]
end

module Make (Incr : Incremental.S) : sig
  module Compare = Compare
  module Collate = Collate
  module Collated = Collated

  (** Perform the filtering, sorting and restricting to ranges.

      [Collate.t] contains the parameters for filtering and sorting, and ranges. It can
      be updated incrementally, but note that filtering & sorting isn't really incremental
      on filter & order, we [bind] to these inside.

      For sorting & filtering, all this function really need is a predicate (i.e. [_ ->
      bool] function) and compare function. However, the interface is slightly different:
      we require user to provide ['filter] and ['order] opaque types in [Collate.t], and
      ways to convert them to predicate & compare here.

      It is done this way for better interaction with [Incr]. We belive that most users
      would have such types, being simple algebraic data types, anyways. You can always
      set e.g. [filter_to_predicate=Fn.id], and just pass the functions directly, but be
      prepared to explore the fascinating world of functions' physical equality.
  *)
  val collate
    :  ?operation_order:[ `Filter_first | `Sort_first ] (** default: `Sort_first *)
    -> ?filter_memoize_params:'filter Incr_memoize.Store_params.t
    (** default: an alist-based LRU with size 1 *)
    -> ?order_memoize_params:'order Incr_memoize.Store_params.t
    (** default: an alist-based LRU with size 10 *)
    -> filter_equal:('filter -> 'filter -> bool)
    -> order_equal:('order -> 'order -> bool)
    -> filter_to_predicate:('filter -> (key:'k -> data:'v -> bool) option)
    -> order_to_compare:('order -> ('k, 'v) Compare.t)
    -> ('k, 'v, 'cmp) Map.t Incr.t
    -> ('k, 'filter, 'order) Collate.t Incr.t
    -> ('k, 'v) Collated.t Incr.t
end
