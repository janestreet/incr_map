open Core
open Import

(** Utility functions for randomly creating and modifying maps with int keys and float
    data between 0 and 1. *)

(** [rand] returns uniformly distributed float between 0 and 1 *)
val rand : unit -> float

(** [init_rand_map] creates a map with keys from [from] (inclusive) to [to_] (exclusive)
    with randomly generated float data *)
val init_rand_map : from:int -> to_:int -> float Int.Map.t

(** [random_add_to_map] randomly chooses a key that is not already in the map and adds it
    to the map with randomly generated float data. *)
val rand_add_to_map : float Int.Map.t -> float Int.Map.t

(** [random_add_to_map_of_vars] is similar to [random_add_to_map], but instead of a
    [float] the data is a [float Incr.Var.t] initialized to a randomly generated float. *)
val rand_add_to_map_of_vars : float Incr.Var.t Int.Map.t -> float Incr.Var.t Int.Map.t

(** [random_replace_in_map] randomly chooses a key in the map and adds it to the map with
    new, randomly generated float data. *)
val rand_replace_in_map : float Int.Map.t -> float Int.Map.t

(** [random_replace_in_map_of_vars] is similar to [random_replace_in_map], but instead of
    a [float], the data is a [float Incr.Var.t] initialized to a randomly generated
    float. *)
val rand_replace_in_map_of_vars : float Incr.Var.t Int.Map.t -> float Incr.Var.t Int.Map.t

(** [random_set_in_map_of_vars] is similar to [random_replace_in_map_of_vars], but instead
    of creating a new [float Incr.Var.t], the existing [float Incr.Var.t] is set to a new
    randomly generated float.  *)
val rand_set_in_map_of_vars : float Incr.Var.t Int.Map.t -> unit

(** [random_remove_from_map] randomly chooses a key in the map and removes it. *)
val rand_remove_from_map : 'a Int.Map.t -> 'a Int.Map.t

(** [rand_modify_map] randomly chooses and calls one of [rand_add_to_map],
    [rand_replace_in_map], and [rand_remove_from_map]. *)
val rand_modify_map : float Int.Map.t -> float Int.Map.t

(** [rand_modify_map_of_vars] randomly chooses and calls one of [rand_add_to_map_of_vars],
    [rand_replace_in_map_of_vars], [rand_set_in_map_of_vars], and
    [rand_remove_from_map_of_vars]. *)
val rand_modify_map_of_vars : float Incr.Var.t Int.Map.t -> float Incr.Var.t Int.Map.t

(* [get_rand_existing_key] returns a random key that is present in the map. *)
val get_rand_existing_key : _ Int.Map.t -> int

(* [get_rand_nonexisting_key] returns a random key that is not present in the
   map. *)
val get_rand_nonexistent_key : _ Int.Map.t -> int
