@@ portable

open! Core

(** [Rank_start] is the simplest kind of key/index, where the first item in a dataset is
    rank 0 and the last item is rank [data_length - 1]. [Incr_map_collate] generally
    normalizes keys to [Rank_from_start] values before doing any work for ease of
    understanding. *)

type t = int

val of_rank : data_length:int -> Collate_params.Rank.t -> t

module Range : sig
  type t = int Maybe_bound.t * int Maybe_bound.t [@@deriving sexp_of, equal]

  val count_before : t -> int

  (** Transform a range of bounds to operate on the input to [basis] rather than the
      output of [basis]. In other words, this util can be used to "merge" ranges and
      combine multiple iterative [subrange_by_rank] filters into one.

      Handles the case where the input [t] has an invalid (negative) start. *)
  val remove_basis : basis:t -> t -> t

  val length : data_length:int -> t -> int

  (** Widens a rank range, and returns the amount that range was widened by. *)
  val widen : by:int * int -> data_length:int -> t -> t * (int * int)

  val of_key_range
    :  key_to_rank_instrumentation:Incr_map.Instrumentation.t option
    -> data:(('k, 'v, 'cmp) Base.Map.t, 'state_witness) Incremental.t
    -> ('k Maybe_bound.t * 'k Maybe_bound.t, 'state_witness) Incremental.t
    -> (t, 'state_witness) Incremental.t

  val of_which_rank_range
    :  data_length:int
    -> Collate_params.Rank.t Collate_params.Which_range.t
    -> t
end
