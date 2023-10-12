open! Core
module Collate = Collate
module Collated = Collated
module Store_params = Incr_memoize.Store_params

module Compare : sig
  (** Note: [Unchanged] and [Reversed] is with respect to ['cmp]. *)
  type ('k, 'v, 'cmp) t =
    | Unchanged
    | Reversed
    | Custom_by_value of { compare : 'v -> 'v -> int }
    | Custom_by_key_and_value of { compare : 'k * 'v -> 'k * 'v -> int }
        (** Partial orders are supported in Custom_by_*, i.e. returning 0 shouldn't cause
        issues. Rows will be then sorted by key. *)
  [@@deriving sexp_of]
end

module Fold : sig
  type ('k, 'v, 'acc) t

  val create
    :  ?revert_to_init_when_empty:bool
    -> init:'acc
    -> add:(key:'k -> data:'v -> 'acc -> 'acc)
    -> ?update:(key:'k -> old_data:'v -> new_data:'v -> 'acc -> 'acc)
    -> remove:(key:'k -> data:'v -> 'acc -> 'acc)
    -> ?finalize:('acc -> 'acc)
    -> unit
    -> ('k, 'v, 'acc) t
end

type ('k, 'v, 'fold_result, 'w) t

(** Perform filtering, sorting and restricting to ranges.

    The [Collate.t Incr.t] contains the parameters for filtering and sorting, and
    ranges. It can be updated incrementally, but note that filtering & sorting isn't
    really incremental on filter & order since we [bind] to these.

    For sorting & filtering, technically all this function should need is a compare
    function and a filtering predicate. However, the interface is slightly different: we
    require users to provide ['filter] and ['order] opaque types in [Collate.t], and
    ways to convert them to predicate & compare here.

    It is done this way for better interaction with [Incr]. We belive that most users
    would have such types, being simple algebraic data types, anyways. You can always
    set e.g. [filter_to_predicate=Fn.id], and just pass the functions directly, but be
    prepared to explore the fascinating world of functions' physical equality. *)
val collate
  :  ?operation_order:[ `Filter_first | `Sort_first ] (** default: `Sort_first *)
  -> filter_equal:('filter -> 'filter -> bool)
  -> order_equal:('order -> 'order -> bool)
  -> filter_to_predicate:('filter -> (key:'k -> data:'v -> bool) option)
  -> order_to_compare:('order -> ('k, 'v, 'cmp) Compare.t)
  -> (('k, 'v, 'cmp) Map.t, 'w) Incremental.t
  -> (('k, 'filter, 'order) Collate.t, 'w) Incremental.t
  -> ('k, 'v, unit, 'w) t

val collate_and_fold
  :  ?operation_order:[ `Filter_first | `Sort_first ] (** default: `Sort_first *)
  -> filter_equal:('filter -> 'filter -> bool)
  -> order_equal:('order -> 'order -> bool)
  -> filter_to_predicate:('filter -> (key:'k -> data:'v -> bool) option)
  -> order_to_compare:('order -> ('k, 'v, 'cmp) Compare.t)
  -> fold:('k, 'v, 'fold_result) Fold.t
  -> (('k, 'v, 'cmp) Map.t, 'w) Incremental.t
  -> (('k, 'filter, 'order) Collate.t, 'w) Incremental.t
  -> ('k, 'v, 'fold_result, 'w) t

(** Gets the collated data produced by a collation function like [collate]. *)
val collated : ('k, 'v, 'fold_result, 'w) t -> (('k, 'v) Collated.t, 'w) Incremental.t

(** A function for finding the index into the collated map of a particular key.
    The resulting index is "pre-range-restriction", which means that even if
    the key is not in the collation range, [key_rank] can still respond with
    its index. However, the index is after filtering and ordering, which means
    that if it is filtered out of the map (or isn't in the original map), then
    the result will be [None]. *)
val key_rank : ('k, 'v, 'fold_result, 'w) t -> ('k -> int option, 'w) Incremental.t

val fold_result : ('k, 'v, 'fold_result, 'w) t -> ('fold_result, 'w) Incremental.t

module With_caching : sig
  (** A version of [collate] with caching.

      We use [Incr_memoize] to cache incremental nodes for the result of a particular:

      - [order],
      - [(order, filter)], and
      - [(order, filter, range_bucket)]

      so that even if the [Collate.t Incr.t] changes, as long as it changes back before
      the result is evicted from the cache, we can resume a cached incremental
      computation instead of discarding it and computing it from scratch.

      Note that if an earlier incremental node is evicted from its cache, its children
      in subsequent caches are no longer used. This is to ensure we don't duplicate
      computations from building later nodes on top of semantically identical but
      physically distinct earlier nodes.

      For example, if the [order] cache is LRU with size 2, and [order_filter_range] is
      LRU with size 10, you could get 10 cached final values if they only use two
      distinct orderings, but if they were to each have a distinct ordering, only two
      will be usable. *)

  module Range_memoize_bucket : sig
    type t [@@deriving sexp_of, equal, hash, compare]

    include Comparable.S_plain with type t := t
  end

  val collate__sort_first
    :  filter_equal:('filter -> 'filter -> bool)
    -> order_equal:('order -> 'order -> bool)
    -> ?order_cache_params:'order Store_params.t (** default: alist of size 10 *)
    -> ?order_filter_cache_params:('order * 'filter) Store_params.t
         (** default: alist of size 30 *)
    -> ?order_filter_range_cache_params:
         ('order * 'filter * Range_memoize_bucket.t) Store_params.t
         (** default: alist of size 50 *)
    -> ?range_memoize_bucket_size:int
    -> filter_to_predicate:('filter -> (key:'k -> data:'v -> bool) option)
    -> order_to_compare:('order -> ('k, 'v, 'cmp) Compare.t)
    -> (('k, 'v, 'cmp) Map.t, 'w) Incremental.t
    -> (('k, 'filter, 'order) Collate.t, 'w) Incremental.t
    -> ('k, 'v, unit, 'w) t

  (** Like [collate__sort_first], but also gives an opportunity to perform a fold over
      the post-filtered, pre-range-restricted data. *)
  val collate_and_fold__sort_first
    :  filter_equal:('filter -> 'filter -> bool)
    -> order_equal:('order -> 'order -> bool)
    -> ?order_cache_params:'order Store_params.t (** default: alist of size 10 *)
    -> ?order_filter_cache_params:('order * 'filter) Store_params.t
         (** default: alist of size 30 *)
    -> ?order_filter_range_cache_params:
         ('order * 'filter * Range_memoize_bucket.t) Store_params.t
         (** default: alist of size
        50 *)
    -> ?range_memoize_bucket_size:int
    -> filter_to_predicate:('filter -> (key:'k -> data:'v -> bool) option)
    -> order_to_compare:('order -> ('k, 'v, 'cmp) Compare.t)
    -> fold:('k, 'v, 'fold_result) Fold.t
    -> (('k, 'v, 'cmp) Map.t, 'w) Incremental.t
    -> (('k, 'filter, 'order) Collate.t, 'w) Incremental.t
    -> ('k, 'v, 'fold_result, 'w) t
end
