open! Core

module Stable = struct
  open Stable_witness.Export

  module Rank = struct
    module V1 = Int.Stable.V1

    module V2 = struct
      type t =
        | From_start of int
        | From_end of int
      [@@deriving
        sexp, compare, equal, bin_io, hash, diff ~stable_version:1, stable_witness]

      let of_v1 v1 = From_start v1
    end
  end

  module Which_range = struct
    module V1 = struct
      type 'a t =
        | All_rows
        | From of 'a
        | To of 'a
        | Between of 'a * 'a
      [@@deriving sexp, compare, equal, bin_io, diff ~stable_version:1, stable_witness]

      let map ~f = function
        | All_rows -> All_rows
        | From a -> From (f a)
        | To a -> To (f a)
        | Between (a, b) -> Between (f a, f b)
      ;;
    end
  end

  module V1 = struct
    type ('k, 'filter, 'order) t =
      { filter : 'filter
      ; order : 'order
      ; key_range : 'k Which_range.V1.t
      ; rank_range : Rank.V1.t Which_range.V1.t
      }
    [@@deriving sexp, equal, bin_io, stable_witness]

    let default ~filter ~order =
      { filter; order; key_range = All_rows; rank_range = All_rows }
    ;;
  end

  module V2 = struct
    type ('k, 'filter, 'order) t =
      { filter : 'filter
      ; order : 'order
      ; key_range : 'k Which_range.V1.t
      ; rank_range : Rank.V2.t Which_range.V1.t
      }
    [@@deriving
      sexp
      , equal
      , bin_io
      , stable_witness
      , stable_record
          ~version:[%stable: ('k, 'filter, 'order) V1.t]
          ~modify:[ rank_range ]]

    let of_v1 v1 = of_V1_t ~modify_rank_range:(Which_range.V1.map ~f:Rank.V2.of_v1) v1

    let default ~filter ~order =
      { filter; order; key_range = All_rows; rank_range = All_rows }
    ;;
  end
end

module Rank = Stable.Rank.V2
module Which_range = Stable.Which_range.V1

type ('k, 'filter, 'order) t = ('k, 'filter, 'order) Stable.V2.t =
  { filter : 'filter
  ; order : 'order
  ; key_range : 'k Which_range.t
  ; rank_range : Rank.t Which_range.t
  }
[@@deriving equal, sexp_of]

let default = Stable.V2.default
let of_stable_v1 = Stable.V2.of_v1
