@@ portable

open! Core

module Rank : sig
  (** Defines an index in the resulting ordered dataset after sorting. The lookup would
      start either from the start of the dataset or from the end. Think of [From_end]
      indices as using negative indices. *)
  type t =
    | From_start of int
    | From_end of int
  [@@deriving sexp, compare, equal, bin_io, hash, diff]
end

module Which_range : sig
  (** Bounds are inclusive at both ends. *)
  type 'a t =
    | All_rows
    | From of 'a
    | To of 'a
    | Between of 'a * 'a
  [@@deriving sexp, compare, equal, bin_io]

  include Diffable.S1 with type 'a t := 'a t

  val map : f:('a -> 'b) -> 'a t -> 'b t
end

type ('k, 'filter, 'order) t =
  { filter : 'filter
  (** User-defined type, usually algebraic data type, describing how the table can be
      filtered. You'll need to provide [equal] and [to_predicate] of type
      ['filter -> ('k -> 'v -> bool)] for [collate]. *)
  ; order : 'order
  (** User-defined type, usually algebraic data type, describing how the table can be
      sorted. You'll need to provide [equal] and [to_compare] of type functionally
      equivalent to ['order -> ('v -> 'v -> int)] for [collate]. *)
  ; key_range : 'k Which_range.t
  (** Select only rows between these keys (in the configured ordering) *)
  ; rank_range : Rank.t Which_range.t
  (** After selecting rows according to [key_range], select rows between these positions.

      For example, if your (sorted & filtered) data is [(A, 1); (B, 2); (C, 3)], then both
      for [{key_range = All_rows; rank_range = (From (From_start 1))}] and
      [{key_range = From B; rank_range = All_rows}] the result would be [(B, 2); (C, 3)] *)
  ; widen_range_by : int * int
  (** An amount to grow the resulting dataset beyond the start and end. Intended for use
      in combination with [key_range], [widen_range_by] makes it possible to include data
      _around_ a range. Negative values are clamped to 0. *)
  }
[@@deriving equal, sexp_of]

val default : filter:'filter -> order:'order -> (_, 'filter, 'order) t

module Stable : sig
  module Rank : sig
    module V1 : module type of Int.Stable.V1

    module V2 : sig
      type t = Rank.t [@@deriving sexp, bin_io, diff, stable_witness]
    end
  end

  module Which_range : sig
    module V1 : sig
      type 'a t = 'a Which_range.t
      [@@deriving sexp, bin_io, compare, equal, diff, stable_witness]
    end
  end

  module V1 : sig
    type ('k, 'filter, 'order) t =
      { filter : 'filter
      ; order : 'order
      ; key_range : 'k Which_range.V1.t
      ; rank_range : Rank.V1.t Which_range.V1.t
      }
    [@@deriving stable_witness, bin_io, sexp, equal]

    val default : filter:'filter -> order:'order -> (_, 'filter, 'order) t
  end

  module V2 : sig
    type ('k, 'filter, 'order) t =
      { filter : 'filter
      ; order : 'order
      ; key_range : 'k Which_range.V1.t
      ; rank_range : Rank.V2.t Which_range.V1.t
      }
    [@@deriving stable_witness, bin_io, sexp, equal]

    val of_v1 : ('k, 'filter, 'order) V1.t -> ('k, 'filter, 'order) t
    val default : filter:'filter -> order:'order -> (_, 'filter, 'order) t
  end

  module V3 : sig
    type nonrec ('k, 'filter, 'order) t = ('k, 'filter, 'order) t
    [@@deriving stable_witness, bin_io, sexp, equal]

    val of_v1 : ('k, 'filter, 'order) V1.t -> ('k, 'filter, 'order) t
    val of_v2 : ('k, 'filter, 'order) V2.t -> ('k, 'filter, 'order) t
    val to_v2 : ('k, 'filter, 'order) t -> ('k, 'filter, 'order) V2.t
    val default : filter:'filter -> order:'order -> (_, 'filter, 'order) t
  end
end

val of_stable_v1 : ('k, 'filter, 'order) Stable.V1.t -> ('k, 'filter, 'order) t
val of_stable_v2 : ('k, 'filter, 'order) Stable.V2.t -> ('k, 'filter, 'order) t
