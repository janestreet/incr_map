open! Core
open! Import
module Incremental_state = (val Incremental.State.create ())

let%expect_test _ =
  let s_var = Incremental.Var.create Incremental_state.t Bool.Map.empty in
  let observer =
    Incremental.observe
      (Incr_set.union_map_data ~comparator:Int.comparator (Incremental.Var.watch s_var))
  in
  let set_key_stabilize_and_show key elements =
    Incremental.Var.replace s_var ~f:(fun s ->
      match elements with
      | None -> Map.remove s key
      | Some elements -> Map.set s ~key ~data:(Int.Set.of_list elements));
    Incremental.stabilize Incremental_state.t;
    print_s [%sexp (Incremental.Observer.value_exn observer : Int.Set.t)]
  in
  set_key_stabilize_and_show false (Some [ 1; 2; 3 ]);
  [%expect {| (1 2 3) |}];
  set_key_stabilize_and_show false (Some [ 1; 2 ]);
  [%expect {| (1 2) |}];
  set_key_stabilize_and_show true (Some [ 2 ]);
  [%expect {| (1 2) |}];
  set_key_stabilize_and_show false None;
  [%expect {| (2) |}];
  set_key_stabilize_and_show true None;
  [%expect {| () |}]
;;

let%test_unit _ =
  let s_var = Incremental.Var.create Incremental_state.t Int.Map.empty in
  let observer =
    Incremental.observe
      (Incr_set.union_map_data ~comparator:Int.comparator (Incremental.Var.watch s_var))
  in
  Quickcheck.test
    (Int.Map.quickcheck_generator
       Quickcheck.Generator.small_non_negative_int
       (Int.Set.quickcheck_generator Quickcheck.Generator.small_non_negative_int))
    ~f:(fun s ->
      Incremental.Var.set s_var s;
      Incremental.stabilize Incremental_state.t;
      let expect = Map.data s |> Set.union_list (module Int) in
      [%test_result: Int.Set.t] ~expect (Incremental.Observer.value_exn observer))
;;
