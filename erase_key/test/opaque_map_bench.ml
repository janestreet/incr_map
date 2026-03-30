open! Core
module Incr = Incr_map_test.Import.Incr

let rs = Random.State.make [| 0 |]
let rand_between a b = Random.State.float_range rs a b

(** A random [int Float.Map.t] which contains [a] and [b] as keys, and [size] total
    elements with keys [a <= key <= b] *)
let rand_float_map_between a b ~size =
  let list = List.map (List.range 1 (size - 1)) ~f:(fun i -> rand_between a b, i) in
  Float.Map.of_alist_exn (((a, 0) :: list) @ [ b, size ])
;;

let bench_addition
  ~start_of_initial_map
  ~end_of_initial_map
  ~initial_map_size
  ~start_of_addition_to_map
  ~end_of_addition_to_map
  ~addition_to_map_size
  =
  let initial_map =
    rand_float_map_between start_of_initial_map end_of_initial_map ~size:initial_map_size
  in
  let additions =
    rand_float_map_between
      start_of_addition_to_map
      end_of_addition_to_map
      ~size:addition_to_map_size
  in
  let input_map = Incr.Var.create initial_map in
  let derived_map =
    Opaque_map.erase_key_incrementally
      ~get:(fun ~key ~data -> key, data)
      (Incr.Var.watch input_map)
  in
  let observer = Incr.observe derived_map in
  Incr.stabilize ();
  fun () ->
    Incr.Var.set input_map initial_map;
    Incr.stabilize ();
    Map.iteri additions ~f:(fun ~key ~data ->
      Incr.Var.set input_map (Map.set (Incr.Var.value input_map) ~key ~data));
    Incr.stabilize ();
    let _derived_map = Sys.opaque_identity (Incr.Observer.value_exn observer) in
    ()
;;

let%bench_fun "fresh_addition" =
  bench_addition
    ~start_of_initial_map:0.0
    ~end_of_initial_map:100.0
    ~initial_map_size:0
    ~start_of_addition_to_map:0.0
    ~end_of_addition_to_map:100.0
    ~addition_to_map_size:10000
;;

let%bench_fun "sub_range_addition" =
  bench_addition
    ~start_of_initial_map:0.0
    ~end_of_initial_map:100.0
    ~initial_map_size:10000
    ~start_of_addition_to_map:25.0
    ~end_of_addition_to_map:75.0
    ~addition_to_map_size:10000
;;

let%bench_fun "super_range_addition" =
  bench_addition
    ~start_of_initial_map:25.0
    ~end_of_initial_map:75.0
    ~initial_map_size:10000
    ~start_of_addition_to_map:0.0
    ~end_of_addition_to_map:100.0
    ~addition_to_map_size:10000
;;

let%bench_fun "prefix_addition" =
  bench_addition
    ~start_of_initial_map:50.0
    ~end_of_initial_map:100.0
    ~initial_map_size:10000
    ~start_of_addition_to_map:0.0
    ~end_of_addition_to_map:50.0
    ~addition_to_map_size:10000
;;

let%bench_fun "suffix_addition" =
  bench_addition
    ~start_of_initial_map:0.0
    ~end_of_initial_map:50.0
    ~initial_map_size:10000
    ~start_of_addition_to_map:50.0
    ~end_of_addition_to_map:100.0
    ~addition_to_map_size:10000
;;

let%bench_fun "injection" =
  bench_addition
    ~start_of_initial_map:0.0
    ~end_of_initial_map:100.0
    ~initial_map_size:2
    ~start_of_addition_to_map:25.0
    ~end_of_addition_to_map:75.0
    ~addition_to_map_size:10000
;;

let get_rand_existing_key map =
  let keys = Map.keys map in
  assert (not (List.is_empty keys));
  List.nth_exn keys (Random.State.int rs (List.length keys))
;;

let rand_remove_from_map map =
  let key = get_rand_existing_key map in
  Map.remove map key
;;

let bench_removal ~initial_map_size ~removal_from_map_size =
  let initial_map = rand_float_map_between 0.0 1.0 ~size:initial_map_size in
  let input_map = Incr.Var.create initial_map in
  let derived_map =
    Opaque_map.erase_key_incrementally
      ~get:(fun ~key ~data -> key, data)
      (Incr.Var.watch input_map)
  in
  let observer = Incr.observe derived_map in
  Incr.stabilize ();
  fun () ->
    Incr.Var.set input_map initial_map;
    Incr.stabilize ();
    for _ = 1 to removal_from_map_size do
      Incr.Var.set input_map (rand_remove_from_map (Incr.Var.value input_map))
    done;
    Incr.stabilize ();
    let _derived_map = Sys.opaque_identity (Incr.Observer.value_exn observer) in
    ()
;;

let%bench_fun "remove_half" =
  bench_removal ~initial_map_size:5000 ~removal_from_map_size:2500
;;

let%bench_fun "remove_all" =
  bench_removal ~initial_map_size:5000 ~removal_from_map_size:5000
;;

let bench_modify ~initial_map_size ~modify_size =
  let initial_map = rand_float_map_between 0.0 1.0 ~size:initial_map_size in
  let key_subset =
    List.fold
      (List.range 0 (initial_map_size - modify_size))
      ~init:initial_map
      ~f:(fun map _i -> rand_remove_from_map map)
    |> Map.keys
  in
  let input_map = Incr.Var.create initial_map in
  let derived_map =
    Opaque_map.erase_key_incrementally
      ~get:(fun ~key ~data -> key, data)
      (Incr.Var.watch input_map)
  in
  let observer = Incr.observe derived_map in
  Incr.stabilize ();
  fun () ->
    Incr.Var.set input_map initial_map;
    Incr.stabilize ();
    List.iteri key_subset ~f:(fun i key ->
      Incr.Var.set input_map (Map.set (Incr.Var.value input_map) ~key ~data:i));
    Incr.stabilize ();
    let _derived_map = Sys.opaque_identity (Incr.Observer.value_exn observer) in
    ()
;;

let%bench_fun "modify_half" = bench_modify ~initial_map_size:5000 ~modify_size:2500
let%bench_fun "modify_all" = bench_modify ~initial_map_size:5000 ~modify_size:5000
