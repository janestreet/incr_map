open Core
open Poly
open Import

let rs = Random.State.make [| 0 |]
let get_rand_key ~max_key = Random.State.int rs max_key
let get_rand_data () = Random.State.float rs 1.
let rand () = Random.State.float rs 1.

let init_rand_map ~from ~to_ =
  let alist = List.map (List.range from to_) ~f:(fun i -> i, get_rand_data ()) in
  Int.Map.of_alist_exn alist
;;

let rec get_rand_nonexistent_key map =
  let key = get_rand_key ~max_key:(max 1000 (Map.length map * 3)) in
  if Map.mem map key then get_rand_nonexistent_key map else key
;;

let get_rand_existing_key map =
  let keys = Map.keys map in
  assert (not (List.is_empty keys));
  List.nth_exn keys (Random.State.int rs (List.length keys))
;;

let rand_add_to_map map =
  let key = get_rand_nonexistent_key map in
  let data = get_rand_data () in
  Map.set map ~key ~data
;;

let rand_replace_in_map map =
  let key = get_rand_existing_key map in
  let data = get_rand_data () in
  Map.set map ~key ~data
;;

let rand_remove_from_map map =
  let key = get_rand_existing_key map in
  Map.remove map key
;;

let rand_modify_map map =
  if Map.is_empty map
  then rand_add_to_map map
  else (
    let rand = rand () in
    if rand < 0.5
    then rand_add_to_map map
    else if rand < 0.75
    then rand_replace_in_map map
    else rand_remove_from_map map)
;;

let rand_add_to_map_of_vars map =
  let key = get_rand_nonexistent_key map in
  let data = Incr.Var.create (get_rand_data ()) in
  Map.set map ~key ~data
;;

let rand_replace_in_map_of_vars map =
  let key = get_rand_existing_key map in
  let data = Incr.Var.create (get_rand_data ()) in
  Map.set map ~key ~data
;;

let rand_set_in_map_of_vars map =
  let key = get_rand_existing_key map in
  let data = Map.find_exn map key in
  Incr.Var.set data (get_rand_data ())
;;

let rand_modify_map_of_vars map =
  if Map.is_empty map
  then rand_add_to_map_of_vars map
  else (
    let rand = rand () in
    if rand < 0.4
    then rand_add_to_map_of_vars map
    else if rand < 0.6
    then (
      let () = rand_set_in_map_of_vars map in
      map)
    else if rand < 0.8
    then rand_replace_in_map_of_vars map
    else rand_remove_from_map map)
;;
