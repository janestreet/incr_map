open! Core
open Import

module Without_stabilize : sig
  type ('key, 'data) t [@@deriving sexp_of]

  val add : key:'key -> data:'data -> ('key, 'data) t
  val remove : 'key -> ('key, _) t
end

type ('key, 'data) t [@@deriving sexp_of]

val add : key:'key -> data:'data -> ('key, 'data) t
val remove : 'key -> ('key, _) t
val stabilize : _ t

val run_operations
  :  ('key, 'data) t list
  -> into:('key, 'data, _) Map.t Incr.Var.t
  -> after_stabilize:(unit -> unit)
  -> unit

val nested_run_operations
  :  ('outer_key, ('inner_key, 'data) Without_stabilize.t list) t list
  -> inner_map_comparator:('inner_key, 'inner_cmp) Comparator.Module.t
  -> into:('outer_key, ('inner_key, 'data, 'inner_cmp) Map.t, _) Map.t Incr.Var.t
  -> after_stabilize:(unit -> unit)
  -> unit

val quickcheck_generator
  :  ?keys_size:int
  -> ?operations:int
  -> 'data Base_quickcheck.Generator.t
  -> (int, 'data) t list Base_quickcheck.Generator.t

val nested_quickcheck_generator
  :  ?outer_map_keys_size:int
  -> ?outer_map_operations:int
  -> ?inner_map_keys_size:int
  -> ?inner_map_operations:int
  -> 'data Base_quickcheck.Generator.t
  -> (int, (int, 'data) Without_stabilize.t list) t list Base_quickcheck.Generator.t

val tuple_key_quickcheck_generator
  :  ?keys_size:int
       (** default is approximately the sqrt of the non-tuple variants to keep the probability
      of modifications approximately equivalent. *)
  -> ?operations:int
  -> 'data Base_quickcheck.Generator.t
  -> (int * int, 'data) t list Base_quickcheck.Generator.t
