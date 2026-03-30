open! Core
module Collate_params = Collate_params
module Collated = Collated

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
