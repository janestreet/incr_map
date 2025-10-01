open! Core
open Incr_map_collate
module Generator = Base_quickcheck.Generator

module Filter : sig
  type t =
    | No_filter
    | Key_has_prefix of string
    | Value_is_even
    | Value_is_odd
  [@@deriving sexp, compare, equal]

  val to_predicate : t -> (key:string -> data:int -> bool) option
end

module Order : sig
  type t =
    | Unchanged
    | Reversed_keys
    | Value_asc
    | Value_desc
  [@@deriving sexp, compare, equal]
end

val small_string_gen : string Generator.t
val small_map_gen : int String.Map.t Generator.t
val key_gen : string Generator.t
val rank_gen : Collate_params.Rank.t Generator.t
val rank_range_gen : Collate_params.Rank.t Collate_params.Which_range.t Generator.t
val key_range_gen : string Collate_params.Which_range.t Generator.t
val filter_gen : Filter.t Generator.t
val order_gen : Order.t Generator.t
val params_gen : (String.t, Filter.t, Order.t) Collate_params.t Generator.t
val operation_order_gen : [ `Filter_first | `Sort_first ] Generator.t
