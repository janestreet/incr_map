open! Core
open Incr_map_collate

(** Non-incremental version of [Incr_map_collate.collate] *)
val collate
  :  operation_order:[ `Filter_first | `Sort_first ]
  -> filter_to_predicate:('filter -> (key:string -> data:int -> bool) option)
  -> order_to_compare:('order -> (string, int, 'cmp) Compare.t)
  -> (string, int, 'cmp) Map.t
  -> (string, 'filter, 'order) Collate_params.t
  -> (string, int) Collated.t
