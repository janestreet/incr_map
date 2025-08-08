open! Core
open! Import
module Incremental_state = (val Incremental.State.create ())

let%expect_test _ =
  let s_var = Incremental.Var.create Incremental_state.t Int.Set.empty in
  let observer =
    Incremental.observe
      (Incr_set.unordered_fold
         (Incremental.Var.watch s_var)
         ~init:0
         ~add:( + )
         ~remove:( - ))
  in
  let stabilize_and_show () =
    Incremental.stabilize Incremental_state.t;
    print_s [%sexp (Incremental.Observer.value_exn observer : int)]
  in
  stabilize_and_show ();
  [%expect {| 0 |}];
  Incremental.Var.set s_var (Int.Set.of_list [ 1; 2; 3 ]);
  stabilize_and_show ();
  [%expect {| 6 |}];
  Incremental.Var.set s_var (Int.Set.of_list [ 1; 3 ]);
  stabilize_and_show ();
  [%expect {| 4 |}];
  Incremental.Var.set s_var (Int.Set.of_list []);
  stabilize_and_show ();
  [%expect {| 0 |}]
;;

let%test_unit _ =
  let s_var = Incremental.Var.create Incremental_state.t Int.Set.empty in
  let observer =
    Incremental.observe
      (Incr_set.unordered_fold
         (Incremental.Var.watch s_var)
         ~init:0
         ~add:( + )
         ~remove:( - ))
  in
  Quickcheck.test
    (Int.Set.quickcheck_generator Quickcheck.Generator.small_non_negative_int)
    ~f:(fun s ->
      Incremental.Var.set s_var s;
      Incremental.stabilize Incremental_state.t;
      [%test_result: int]
        ~expect:(Set.fold ~init:0 ~f:( + ) s)
        (Incremental.Observer.value_exn observer))
;;
