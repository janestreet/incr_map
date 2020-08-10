open! Core
open! Import
module M = Rand_map_helper

let assert_same ~iterations ~stabilize_every ~mutator =
  let map = M.init_rand_map ~from:25 ~to_:150 in
  let key = M.get_rand_existing_key map in
  let map_var = Incr.Var.create map in
  let key_var = Incr.Var.create key in
  let output = Incr_map.rank (Incr.Var.watch map_var) (Incr.Var.watch key_var) in
  let observed = Incr.observe output in
  (* this old_key ref is just for debugging *)
  let old_key = ref key in
  for i = 0 to iterations do
    let new_map, new_key = mutator (Incr.Var.value map_var) (Incr.Var.value key_var) in
    Incr.Var.set map_var new_map;
    Incr.Var.set key_var new_key;
    if 0 = i mod stabilize_every
    then (
      Incr.stabilize ();
      let rank_computed_naively = Map.rank new_map new_key in
      let rank_computed_incrementally = Incr.Observer.value_exn observed in
      [%test_result: int option] rank_computed_incrementally ~expect:rank_computed_naively;
      old_key := new_key)
  done
;;

let test ~mutator =
  for _ = 0 to 1000 do
    for stabilize_every = 1 to 5 do
      assert_same ~iterations:100 ~stabilize_every ~mutator
    done
  done
;;

let%test_unit "make no changes" = test ~mutator:(fun map key -> map, key)
let%test_unit "change map to empty" = test ~mutator:(fun _ key -> Int.Map.empty, key)

let%test_unit "remove entries from map" =
  test ~mutator:(fun map key -> M.rand_remove_from_map map, key)
;;

let%test_unit "add entries to map" =
  test ~mutator:(fun map key -> M.rand_add_to_map map, key)
;;

let%test_unit "add and remove entries from map" =
  let open Poly in
  test ~mutator:(fun map key ->
    let map =
      if M.rand () < 0.5 then M.rand_add_to_map map else M.rand_add_to_map map
    in
    map, key)
;;

let%test_unit "modify map" = test ~mutator:(fun map key -> M.rand_modify_map map, key)

let%test_unit "if the key is in the map, remove it from the map, otherwise pick a new key"
  =
  test ~mutator:(fun map key ->
    if Map.mem map key
    then Map.remove map key, key
    else map, M.get_rand_existing_key map)
;;

let%test_unit "pick random keys" =
  test ~mutator:(fun map _key -> map, M.get_rand_existing_key map)
;;

let%test_unit "remove elements from map and pick random keys" =
  test ~mutator:(fun map _key ->
    let map = M.rand_remove_from_map map in
    map, M.get_rand_existing_key map)
;;

let%test_unit "remove elements from map and pick random keys, but with a probability of \
               picking the key that was just removed, oh no!"
  =
  test ~mutator:(fun map _key -> M.rand_remove_from_map map, M.get_rand_existing_key map)
;;

let%test_unit "add elements to map and pick random keys, but with a probability of \
               picking the key that was just added, oh no!"
  =
  test ~mutator:(fun map _key ->
    let map = M.rand_add_to_map map in
    map, M.get_rand_existing_key map)
;;

let%test_unit "add elements to map and pick random keys" =
  test ~mutator:(fun map _key -> M.rand_add_to_map map, M.get_rand_existing_key map)
;;

let%test_unit "modify map and pick random keys from new map" =
  test ~mutator:(fun map _key ->
    let map = M.rand_modify_map map in
    map, M.get_rand_existing_key map)
;;

let%test_unit "modify map and pick random keys from old map" =
  test ~mutator:(fun map _key -> M.rand_modify_map map, M.get_rand_existing_key map)
;;

let%test_unit "modify map and pick random keys, but with a 50% probabily of the key not \
               being in new map"
  =
  test ~mutator:(fun map _key ->
    let open Poly in
    let map = M.rand_modify_map map in
    let key =
      if M.rand () < 0.5
      then M.get_rand_existing_key map
      else M.get_rand_nonexistent_key map
    in
    map, key)
;;
