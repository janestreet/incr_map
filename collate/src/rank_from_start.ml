open! Core
open Incremental.Let_syntax
include Collate_shared.Rank_from_start

module Range = struct
  include Collate_shared.Rank_from_start.Range

  let of_key_range
    (type k v cmp state_witness)
    ~key_to_rank_instrumentation
    ~(data : ((k, v, cmp) Map.t, state_witness) Incremental.t)
    (key_range : (k Maybe_bound.t * k Maybe_bound.t, state_witness) Incremental.t)
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
      Incremental.map2
        (to_rank_bound start_bound)
        (to_rank_bound end_bound)
        ~f:(fun lo hi -> create ~lo ~hi)
  ;;
end
