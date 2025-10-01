open! Core
open! Import
module Incremental_state = (val Incremental.State.create ())

let%expect_test _ =
  let s_var = Incremental.Var.create Incremental_state.t Int.Set.empty in
  let f = Int.is_positive in
  let observer = Incremental.observe (Incr_set.filter (Incremental.Var.watch s_var) ~f) in
  let stabilize_and_show () =
    Incremental.stabilize Incremental_state.t;
    print_s [%sexp (Incremental.Observer.value_exn observer : Int.Set.t)]
  in
  stabilize_and_show ();
  [%expect {| () |}];
  Incremental.Var.set s_var (Int.Set.of_list [ 1; 2; 3 ]);
  stabilize_and_show ();
  [%expect {| (1 2 3) |}];
  Incremental.Var.set s_var (Int.Set.of_list [ 1; 2; -3 ]);
  stabilize_and_show ();
  [%expect {| (1 2) |}];
  Incremental.Var.set s_var (Int.Set.of_list [ -1; 2; -3 ]);
  stabilize_and_show ();
  [%expect {| (2) |}];
  Incremental.Var.set s_var (Int.Set.of_list [ -1; -2; -3 ]);
  stabilize_and_show ();
  [%expect {| () |}];
  Incremental.Var.set s_var Int.Set.empty;
  stabilize_and_show ();
  [%expect {| () |}]
;;

let%test_unit _ =
  let s_var = Incremental.Var.create Incremental_state.t Int.Set.empty in
  let f x = Int.(x % 2 = 0) in
  let observer = Incremental.observe (Incr_set.filter (Incremental.Var.watch s_var) ~f) in
  Quickcheck.test
    (Int.Set.quickcheck_generator Quickcheck.Generator.small_non_negative_int)
    ~f:(fun s ->
      Incremental.Var.set s_var s;
      Incremental.stabilize Incremental_state.t;
      [%test_result: Int.Set.t]
        ~expect:(Set.filter s ~f)
        (Incremental.Observer.value_exn observer))
;;
