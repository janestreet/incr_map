open! Core
open! Import
module Incremental_state = (val Incremental.State.create ())

let%expect_test _ =
  let s1_var = Incremental.Var.create Incremental_state.t Int.Set.empty in
  let s2_var = Incremental.Var.create Incremental_state.t Int.Set.empty in
  let observer =
    Incremental.observe
      (Incr_set.diff (Incremental.Var.watch s1_var) (Incremental.Var.watch s2_var))
  in
  let stabilize_and_show () =
    Incremental.stabilize Incremental_state.t;
    print_s [%sexp (Incremental.Observer.value_exn observer : Int.Set.t)]
  in
  stabilize_and_show ();
  [%expect {| () |}];
  Incremental.Var.set s1_var (Int.Set.of_list [ 1; 2 ]);
  stabilize_and_show ();
  [%expect {| (1 2) |}];
  Incremental.Var.set s2_var (Int.Set.singleton 2);
  stabilize_and_show ();
  [%expect {| (1) |}];
  Incremental.Var.set s2_var Int.Set.empty;
  stabilize_and_show ();
  [%expect {| (1 2) |}];
  Incremental.Var.set s2_var (Int.Set.singleton 3);
  stabilize_and_show ();
  [%expect {| (1 2) |}];
  Incremental.Var.set s1_var (Int.Set.of_list [ 1; 2; 3 ]);
  stabilize_and_show ();
  [%expect {| (1 2) |}]
;;

let%test_unit _ =
  let s1_var = Incremental.Var.create Incremental_state.t Int.Set.empty in
  let s2_var = Incremental.Var.create Incremental_state.t Int.Set.empty in
  let observer =
    Incremental.observe
      (Incr_set.diff (Incremental.Var.watch s1_var) (Incremental.Var.watch s2_var))
  in
  Quickcheck.test
    (Quickcheck.Generator.tuple2
       (Int.Set.quickcheck_generator Quickcheck.Generator.small_non_negative_int)
       (Int.Set.quickcheck_generator Quickcheck.Generator.small_non_negative_int))
    ~f:(fun (s1, s2) ->
      Incremental.Var.set s1_var s1;
      Incremental.Var.set s2_var s2;
      Incremental.stabilize Incremental_state.t;
      [%test_result: Int.Set.t]
        ~expect:(Set.diff s1 s2)
        (Incremental.Observer.value_exn observer))
;;
