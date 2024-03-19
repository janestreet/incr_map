open! Core

module Stable = struct
  open Stable_witness.Export

  module Which_range = struct
    module V1 = struct
      type 'a t =
        | All_rows
        | From of 'a
        | To of 'a
        | Between of 'a * 'a
      [@@deriving sexp, compare, equal, bin_io, diff ~stable_version:1, stable_witness]
    end
  end

  module V1 = struct
    type ('k, 'filter, 'order) t =
      { filter : 'filter
      ; order : 'order
      ; key_range : 'k Which_range.V1.t
      ; rank_range : int Which_range.V1.t
      }
    [@@deriving sexp, bin_io, stable_witness]
  end
end

module Which_range = Stable.Which_range.V1

type ('k, 'filter, 'order) t = ('k, 'filter, 'order) Stable.V1.t =
  { filter : 'filter
  ; order : 'order
  ; key_range : 'k Which_range.t
  ; rank_range : int Which_range.t
  }
[@@deriving equal, sexp_of]

let default ~filter ~order =
  { filter; order; key_range = All_rows; rank_range = All_rows }
;;
