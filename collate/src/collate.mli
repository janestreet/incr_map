open! Core_kernel

module Which_range : sig
  (** Bounds are inclusive at both ends. *)
  type 'a t =
    | All_rows
    | From of 'a
    | To of 'a
    | Between of 'a * 'a
  [@@deriving sexp, compare, equal, bin_io]
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
  ; rank_range : int Which_range.t
  (** After selecting rows according to [key_range], select rows between these
      positions.

      For example, if your (sorted & filtered) data is [(A, 1); (B, 2); (C, 3)], then
      both for
      {key_range = All_rows; rank_range = From 1}
      and
      {key_range = From B; rank_range = All_rows}
      the result would be [(B, 2); (C, 3)]
  *)
  }
[@@deriving sexp_of, fields]

val default : filter:'filter -> order:'order -> (_, 'filter, 'order) t
