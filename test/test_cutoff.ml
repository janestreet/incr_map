open! Core
open! Import

let cutoff = Incremental.Cutoff.of_equal (fun a b -> Float.(abs (a - b) < 1.0))

let run_operations ~test_impl ~reference_impl ~f operations =
  let var = Incr.Var.create Int.Map.empty in
  let reference_observer = Incremental.observe (reference_impl (Incr.Var.watch var)) in
  let test_observer = Incremental.observe (test_impl (Incr.Var.watch var)) in
  Map_operations.run_operations ~into:var operations ~after_stabilize:(fun () ->
    let reference_v = Incremental.Observer.value_exn reference_observer in
    let test_v = Incremental.Observer.value_exn test_observer in
    f ~test_v ~reference_v)
;;

let run_test ~test_impl ~reference_impl =
  let int_map_operations_generator =
    Map_operations.quickcheck_generator
      ~keys_size:10
      ~operations:100
      (Base_quickcheck.Generator.float_inclusive 0.0 10.0)
  in
  Quickcheck.test
    ~seed:(`Deterministic "this is the seed")
    int_map_operations_generator
    ~f:
      (run_operations ~test_impl ~reference_impl ~f:(fun ~test_v ~reference_v ->
         [%test_result: float Int.Map.t] test_v ~expect:reference_v))
;;

let reference_impl input =
  Incr_map.map' input ~f:(fun v ->
    Incr.set_cutoff v cutoff;
    v)
;;

let%test_unit "quickcheck [Incr_map.cutoff] vs Map' with Incr.cutoff" =
  let test_impl input = Incr_map.cutoff input ~cutoff in
  run_test ~reference_impl ~test_impl
;;

let%expect_test "naive data_equal" =
  (*=You might expect this code to be equivalent to [Incr_map.cutoff]:

     {[
       Incr_map.map
         input
         ~data_equal:(fun old_value new_value ->
           Incremental.Cutoff.should_cutoff cutoff ~old_value ~new_value)
         ~f:Fn.id
     ]}

     but it does not, because [map] always diffs the current map against the
     previous _input_ to the map, which can cause cutoff sliding.  Let's say
     that our cutoff function is

     | a - b | <= 1

     and we have a map whose contents move from

     {v
       { a => 1 }
     to
       { a => 2 }
     to
       { a => 3 }
     v}

     The implementataion using [Incr_map.map'] and [Incr_map.cutoff] correctly report
     the map as changing from { a => 1 } -> { a => 1 } -> { a => 3 }, with the first
     update being blocked by the cutoff function, but the [Incr_map.map ~data_equal]
     implementaion will report { a => 1 } -> { a => 1 } -> { a => 1 }. *)
  let test_impl input =
    Incr_map.map
      input
      ~data_equal:(fun old_value new_value ->
        Incremental.Cutoff.should_cutoff cutoff ~old_value ~new_value)
      ~f:Fn.id
  in
  let operations =
    Map_operations.
      [ add ~key:0 ~data:0.0
      ; stabilize
      ; add ~key:0 ~data:0.5
      ; stabilize
      ; add ~key:0 ~data:1.0
      ; stabilize
      ; add ~key:0 ~data:1.5
      ; stabilize
      ; add ~key:0 ~data:2.0
      ; stabilize
      ]
  in
  print_endline "     ref vs test";
  run_operations operations ~test_impl ~reference_impl ~f:(fun ~test_v ~reference_v ->
    let test_value = Map.find_exn test_v 0 in
    let reference_value = Map.find_exn reference_v 0 in
    printf "%f vs %f\n" reference_value test_value);
  [%expect
    {|
         ref vs test
    0.000000 vs 0.000000
    0.000000 vs 0.000000
    1.000000 vs 0.000000
    1.000000 vs 0.000000
    2.000000 vs 0.000000
    |}]
;;
