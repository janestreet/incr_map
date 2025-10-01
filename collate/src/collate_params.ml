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

  module V3 = struct
    type ('k, 'filter, 'order) t =
      { filter : 'filter
      ; order : 'order
      ; key_range : 'k Which_range.V1.t
      ; rank_range : Rank.V2.t Which_range.V1.t
      ; widen_range_by : int * int
      }
    [@@deriving
      sexp
      , equal
      , bin_io
      , stable_witness
      , stable_record
          ~version:[%stable: ('k, 'filter, 'order) V2.t]
          ~remove:[ widen_range_by ]]

    (* NB: tuples use structural equality so we want to always return the same thing *)
    let default_widen_range_by = 0, 0
    let of_v2 v2 = of_V2_t ~widen_range_by:default_widen_range_by v2
    let to_v2 = to_V2_t
    let of_v1 v1 = V2.of_v1 v1 |> of_v2

    let default ~filter ~order =
      { filter
      ; order
      ; key_range = All_rows
      ; rank_range = All_rows
      ; widen_range_by = default_widen_range_by
      }
    ;;
  end
end

module Rank = Stable.Rank.V2
module Which_range = Stable.Which_range.V1

type ('k, 'filter, 'order) t = ('k, 'filter, 'order) Stable.V3.t =
  { filter : 'filter
  ; order : 'order
  ; key_range : 'k Which_range.t
  ; rank_range : Rank.t Which_range.t
  ; widen_range_by : int * int
  }
[@@deriving equal, sexp_of]

let default = Stable.V3.default
let of_stable_v1 = Stable.V3.of_v1
let of_stable_v2 = Stable.V3.of_v2
