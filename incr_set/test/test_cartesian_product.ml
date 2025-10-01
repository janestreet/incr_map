open! Core
open! Import
module Incremental_state = (val Incremental.State.create ())

let%expect_test _ =
  let s1_var = Incremental.Var.create Incremental_state.t Int.Set.empty in
  let s2_var = Incremental.Var.create Incremental_state.t Int.Set.empty in
  let observer =
    Incremental.observe
      (Incr_set.cartesian_product
         (Incremental.Var.watch s1_var)
         (Incremental.Var.watch s2_var))
  in
  let stabilize_and_show () =
    Incremental.stabilize Incremental_state.t;
    print_s
      [%sexp (Incremental.Observer.value_exn observer |> Set.to_list : (int * int) list)]
  in
  stabilize_and_show ();
  [%expect {| () |}];
  Incremental.Var.set s1_var (Int.Set.of_list [ 1; 2 ]);
  stabilize_and_show ();
  [%expect {| () |}];
  Incremental.Var.set s2_var (Int.Set.singleton 2);
  stabilize_and_show ();
  [%expect {| ((1 2) (2 2)) |}];
  Incremental.Var.set s1_var (Int.Set.of_list [ 2; 3 ]);
  stabilize_and_show ();
  [%expect {| ((2 2) (3 2)) |}];
  Incremental.Var.set s2_var (Int.Set.of_list [ 4; 5 ]);
  stabilize_and_show ();
  [%expect {| ((2 4) (2 5) (3 4) (3 5)) |}];
  Incremental.Var.set s1_var Int.Set.empty;
  stabilize_and_show ();
  [%expect {| () |}];
  Incremental.Var.set s1_var (Int.Set.singleton 3);
  stabilize_and_show ();
  [%expect {| ((3 4) (3 5)) |}]
;;

let%test_unit _ =
  let s1_var = Incremental.Var.create Incremental_state.t Int.Set.empty in
  let s2_var = Incremental.Var.create Incremental_state.t Int.Set.empty in
  let observer =
    Incremental.observe
      (Incr_set.cartesian_product
         (Incremental.Var.watch s1_var)
         (Incremental.Var.watch s2_var))
  in
  let all_at_once_impl s1 s2 =
    List.cartesian_product (Set.to_list s1) (Set.to_list s2)
    |> Set.of_list (module Tuple.Comparator (Int) (Int))
  in
  Quickcheck.test
    (Quickcheck.Generator.tuple2
       (Int.Set.quickcheck_generator Quickcheck.Generator.small_non_negative_int)
       (Int.Set.quickcheck_generator Quickcheck.Generator.small_non_negative_int))
    ~f:(fun (s1, s2) ->
      Incremental.Var.set s1_var s1;
      Incremental.Var.set s2_var s2;
      Incremental.stabilize Incremental_state.t;
      [%test_result: (int * int) list]
        ~expect:(all_at_once_impl s1 s2 |> Set.to_list)
        (Incremental.Observer.value_exn observer |> Set.to_list))
;;
