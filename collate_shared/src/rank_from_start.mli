@@ portable

open! Core

(** [Rank_from_start] is the simplest kind of key/index, where the first item in a dataset
    is rank 0 and the last item is rank [data_length - 1]. Collation generally normalizes
    keys to [Rank_from_start] values before doing any work for ease of understanding. *)

type t = int

val of_rank : data_length:int -> Collate_protocol.Collate_params.Rank.t -> t

module Range : sig
  type t = int Maybe_bound.t * int Maybe_bound.t [@@deriving sexp_of, equal]

  val create : lo:int Maybe_bound.t -> hi:int Maybe_bound.t -> t
  val count_before : t -> int

  (** Transform a range of bounds to operate on the input to [basis] rather than the
      output of [basis]. In other words, this util can be used to "merge" ranges and
      combine multiple iterative [subrange_by_rank] filters into one.

      Handles the case where the input [t] has an invalid (negative) start. *)
  val remove_basis : basis:t -> t -> t

  val length : data_length:int -> t -> int

  (** Convert to a start index (inclusive) and end index (exclusive), clamped to valid
      indices within 0 to data_length. *)
  val to_index_range : data_length:int -> t -> int * int

  (** Widens a rank range, and returns the amount that range was widened by. *)
  val widen : by:int * int -> data_length:int -> t -> t * (int * int)

  val of_which_rank_range
    :  data_length:int
    -> Collate_protocol.Collate_params.Rank.t
         Collate_protocol.Collate_params.Which_range.t
    -> t
end
