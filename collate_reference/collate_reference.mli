open! Core

(** Non-incremental version of [Incr_map_collate.collate] *)
val collate
  :  operation_order:[ `Filter_first | `Sort_first ]
  -> filter_to_predicate:('filter -> (key:string -> data:int -> bool) option)
  -> order_to_compare:('order -> (string, int, 'cmp) Collate_protocol.Compare.t)
  -> (string, int, 'cmp) Map.t
  -> (string, 'filter, 'order) Collate_protocol.Collate_params.t
  -> (string, int) Collate_protocol.Collated.t
