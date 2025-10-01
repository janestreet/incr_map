open! Core
open Incremental.Let_syntax

type t = int

let of_rank ~data_length = function
  | Collate_params.Rank.From_start i -> i
  | From_end i -> data_length - i - 1
;;

type 'a range = 'a Maybe_bound.t * 'a Maybe_bound.t [@@deriving sexp_of, equal]

(** Apply an offset to a [Maybe_bound.t], returning the updated [Maybe_bound.t] and the
    actual offset applied *)
let shift_rank_bound ~by ~max_rank = function
  | Maybe_bound.Unbounded -> Unbounded, 0
  | Incl i ->
    (* User may pass in an invalid rank, we don't want to truncate it unnecessarily *)
    let max_rank = Int.max i max_rank in
    let min_rank = Int.min i 0 in
    let i_offset = Int.clamp_exn ~min:min_rank ~max:max_rank (i + by) in
    Incl i_offset, i_offset - i
  | Excl i ->
    (* User may pass in an invalid rank, we don't want to truncate it unnecessarily *)
    let max_excl = Int.max i (max_rank + 1) in
    let min_excl = Int.min i (-1) in
    let i_offset = Int.clamp_exn ~min:min_excl ~max:max_excl (i + by) in
    Excl i_offset, i_offset - i
;;

module Range = struct
  type t = int range [@@deriving sexp_of, equal]

  let count_before = function
    | Maybe_bound.Unbounded, _ -> 0
    | Incl i, _ -> Int.max 0 i
    | Excl i, _ -> Int.max 0 (i + 1)
  ;;

  let length ~data_length t =
    let l, u = t in
    let length_to = function
      | Maybe_bound.Unbounded -> data_length
      | Incl i -> i + 1
      | Excl i -> i
    in
    match l with
    | Maybe_bound.Unbounded -> length_to u
    | Incl l -> length_to u - l
    | Excl l -> length_to u - l + 1
  ;;

  (* Returns the lower-ranked bound *)
  let min_bound b1 b2 =
    match b1, b2 with
    | Unbounded, _ -> b2
    | _, Unbounded -> b1
    | Incl l1, Incl l2 -> Incl (Int.min l1 l2)
    | Incl l1, Excl l2 -> Excl (Int.min (l1 + 1) l2)
    | Excl l1, Incl l2 -> Excl (Int.min l1 (l2 + 1))
    | Excl l1, Excl l2 -> Excl (Int.min l1 l2)
  ;;

  let remove_basis ~basis t =
    let basis_start, basis_end = basis in
    let start_offset =
      (match basis_start with
       | Unbounded -> 0
       | Incl l -> l
       | Excl l -> l + 1)
      |> Int.max 0
    in
    let start =
      match t with
      | Unbounded, _ -> basis_start
      | Incl start, _ ->
        let start = Int.max 0 start in
        Incl (start + start_offset)
      | Excl start, _ ->
        let start = Int.max (-1) start in
        Excl (start + start_offset)
    in
    let end_ =
      match t with
      | _, Unbounded -> basis_end
      | _, Incl end_ -> Incl (end_ + start_offset)
      | _, Excl end_ -> Excl (end_ + start_offset)
    in
    start, min_bound basis_end end_
  ;;

  let widen ~by ~data_length range =
    let by_before, by_after = by in
    let l, u = range in
    let max_rank = data_length - 1 in
    let start_bound, shifted_before_by = shift_rank_bound ~by:(-by_before) ~max_rank l in
    let widened_before_by = -shifted_before_by in
    let end_bound, widened_after_by = shift_rank_bound ~by:by_after ~max_rank u in
    (start_bound, end_bound), (widened_before_by, widened_after_by)
  ;;

  let of_key_range
    (type k v cmp state_witness)
    ~key_to_rank_instrumentation
    ~(data : ((k, v, cmp) Map.t, state_witness) Incremental.t)
    (key_range : (k range, state_witness) Incremental.t)
    : (t, state_witness) Incremental.t
    =
    let incremental_state = Incremental.state key_range in
    let unbounded = Incremental.return incremental_state Unbounded in
    let to_rank_bound key_bound =
      match%pattern_bind key_bound with
      | Maybe_bound.Incl l ->
        (match%pattern_bind
           Incr_map.rank ?instrumentation:key_to_rank_instrumentation data l
         with
         | Some l ->
           let%map.Incremental l in
           Maybe_bound.Incl l
         | None -> unbounded)
      | Excl l ->
        (match%pattern_bind
           Incr_map.rank ?instrumentation:key_to_rank_instrumentation data l
         with
         | Some l ->
           let%map.Incremental l in
           Excl l
         | None -> unbounded)
      | Unbounded -> unbounded
    in
    match%pattern_bind key_range with
    | start_bound, end_bound ->
      Incremental.both (to_rank_bound start_bound) (to_rank_bound end_bound)
  ;;

  let of_which_rank_range ~data_length which_rank_range =
    match which_rank_range with
    | Collate_params.Which_range.All_rows -> Maybe_bound.Unbounded, Maybe_bound.Unbounded
    | Between (Collate_params.Rank.From_start _, From_start _) as range ->
      (match range with
       | Between (From_start l, From_start u) -> Incl l, Incl u
       | _ -> assert false)
    | From (From_start l) -> Incl l, Unbounded
    | To (From_start u) -> Unbounded, Incl u
    | Between _ as range ->
      (match range with
       | Between (l, u) -> Incl (of_rank ~data_length l), Incl (of_rank ~data_length u)
       | _ -> assert false)
    | From l -> Incl (of_rank ~data_length l), Unbounded
    | To u -> Unbounded, Incl (of_rank ~data_length u)
  ;;
end
