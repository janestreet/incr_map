open! Core
open Import

type 'a t [@@deriving sexp_of]

val run_operations
  :  'a t list
  -> into:'a Core.Int.Map.t Incr.Var.t
  -> after_stabilize:(unit -> unit)
  -> unit

val quickcheck_generator
  :  ?keys_size:int
  -> ?operations:int
  -> 'a Base_quickcheck.Generator.t
  -> 'a t list Base_quickcheck.Generator.t
