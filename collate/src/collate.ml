open! Core_kernel

module Which_range = struct
  type 'a t =
    | All_rows
    | From of 'a
    | To of 'a
    | Between of 'a * 'a
  [@@deriving sexp, compare, equal, bin_io]
end

type ('k, 'filter, 'order) t =
  { filter : 'filter
  ; order : 'order
  ; key_range : 'k Which_range.t
  ; rank_range : int Which_range.t
  }
[@@deriving sexp_of, fields]

let default ~filter ~order =
  { filter; order; key_range = All_rows; rank_range = All_rows }
;;
