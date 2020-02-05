open Core
open Import
open Incr_map_test.Subrange_quickcheck_helper

let global_seed_for_benchmarks = "31415925..."

let%bench_module "inline_benchmarks" =
  (module struct
    let setup_incr map ~range =
      let map_var = Incr.Var.create map in
      let range_var = Incr.Var.create range in
      let sub =
        Incr.Map.subrange_by_rank (Incr.Var.watch map_var) (Incr.Var.watch range_var)
      in
      let obs = Incr.observe sub in
      map_var, range_var, obs
    ;;

    let setup_rand ~size ~num_ops =
      let seed = `Deterministic global_seed_for_benchmarks in
      let map = Quickcheck.random_value ~seed (map_with_length_gen size) in
      let ops = Quickcheck.random_sequence ~seed (map_op_gen ()) in
      let ops = Sequence.take ops num_ops |> Sequence.force_eagerly in
      map, ops
    ;;

    let apply_ops ~init ops =
      (* Applying ops is costly, so we just accumulate all maps and ranges in advance. *)
      Sequence.fold ops ~init:[ init ] ~f:(fun states (map_op, range_op) ->
        let map, range = List.hd_exn states in
        (apply_map_op map map_op, apply_range_op range range_op) :: states)
    ;;

    let benchmark_fun states map_var range_var obs () =
      let map, range = Sequence.hd_exn !states in
      states := Sequence.tl_eagerly_exn !states;
      Incr.Var.set map_var map;
      Incr.Var.set range_var range;
      Incr.stabilize ();
      let (_ : map) = Incr.Observer.value_exn obs in
      ()
    ;;

    let benchmark_fixed_range ~range ~size ~num_ops =
      let map, ops = setup_rand ~size ~num_ops in
      let ops = Sequence.map ~f:(fun map_op -> map_op, `No_change) ops in
      let map_var, range_var, obs = setup_incr map ~range in
      let states = ref (Sequence.cycle_list_exn (apply_ops ~init:(map, range) ops)) in
      Incr.stabilize ();
      let (_ : map) = Incr.Observer.value_exn obs in
      benchmark_fun states map_var range_var obs
    ;;

    let benchmark_fixed_map ~range ~new_range ~size =
      let map, _ = setup_rand ~size ~num_ops:0 in
      let map_var, range_var, obs = setup_incr map ~range in
      let ops = Sequence.of_list [ `No_change, `Set_range new_range ] in
      let states = ref (Sequence.cycle_list_exn (apply_ops ~init:(map, range) ops)) in
      Incr.stabilize ();
      let (_ : map) = Incr.Observer.value_exn obs in
      benchmark_fun states map_var range_var obs
    ;;

    let benchmark_fixed_range_from_scratch ~range ~size ~num_ops =
      let map, ops = setup_rand ~size ~num_ops in
      let ops = Sequence.map ~f:(fun map_op -> map_op, `No_change) ops in
      let states = ref (Sequence.cycle_list_exn (apply_ops ~init:(map, range) ops)) in
      fun () ->
        let map_var, range_var, obs = setup_incr map ~range in
        benchmark_fun states map_var range_var obs ();
        Incr.Observer.disallow_future_use obs
    ;;

    let size = 1_000_000
    let num_ops = 200

    let%bench_fun "first page" =
      benchmark_fixed_range ~range:(Incl 0, Incl 30) ~size ~num_ops
    ;;

    let%bench_fun "first page from scratch" =
      benchmark_fixed_range_from_scratch ~range:(Incl 0, Incl 30) ~size ~num_ops
    ;;

    let%bench_fun "middle page" =
      benchmark_fixed_range
        ~range:(Incl (size / 2), Incl ((size / 2) + 30))
        ~size
        ~num_ops
    ;;

    let%bench_fun "middle page from scratch" =
      benchmark_fixed_range_from_scratch
        ~range:(Incl (size / 2), Incl ((size / 2) + 30))
        ~size
        ~num_ops
    ;;

    let%bench_fun "last page" =
      benchmark_fixed_range ~range:(Incl (size - 31), Incl (size - 1)) ~size ~num_ops
    ;;

    let%bench_fun "last page from scratch" =
      benchmark_fixed_range_from_scratch
        ~range:(Incl (size - 31), Incl (size - 1))
        ~size
        ~num_ops
    ;;

    let map =
      Core_kernel.Int.Map.of_increasing_sequence
        (Sequence.range 0 1_000_000 |> Sequence.map ~f:(fun x -> x, ()))
      |> Or_error.ok_exn
    ;;

    let%bench_fun "next page" =
      let x = 500_000 in
      benchmark_fixed_map
        ~range:(Incl x, Incl (x + 30))
        ~new_range:(Incl (x + 31), Incl (x + 60))
        ~size:1_000_000
    ;;

    let find_key_range_linear ~from ~to_ () =
      let (_ : (int * int option) option) =
        Incr.Map.For_testing.find_key_range_linear ~from ~to_ map
      in
      ()
    ;;

    (* Test that beginning and end is fast, and middle is slow *)

    let%bench_fun "find_key_range_linear beginning" =
      find_key_range_linear ~from:0 ~to_:30
    ;;

    let%bench_fun "find_key_range_linear middle" =
      find_key_range_linear ~from:500_000 ~to_:500_030
    ;;

    let%bench_fun "find_key_range_linear end" =
      find_key_range_linear ~from:999_970 ~to_:1_000_000
    ;;
  end)
;;
