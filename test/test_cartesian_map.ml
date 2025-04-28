open! Core
open Import

let%expect_test "cartesian_product" =
  let m1_var = Incr.Var.create String.Map.empty in
  let m2_var = Incr.Var.create Char.Map.empty in
  let observer =
    Incr.observe
      (Incr_map.cartesian_product
         (Incr.Var.watch m1_var)
         (Incr.Var.watch m2_var)
         ~data_equal_left:String.equal
         ~data_equal_right:equal)
  in
  let stabilize_and_show () =
    Incr.stabilize ();
    print_s
      [%sexp
        (Incr.Observer.value_exn observer |> Map.to_alist
         : ((String.t * Char.t) * (String.t * int)) list)]
  in
  stabilize_and_show ();
  [%expect {| () |}];
  Incr.Var.set m1_var (String.Map.of_alist_exn [ "apple", "red"; "banana", "yellow" ]);
  stabilize_and_show ();
  [%expect {| () |}];
  Incr.Var.set m2_var (Char.Map.of_alist_exn [ 'X', 200 ]);
  stabilize_and_show ();
  [%expect
    {|
    (((apple  X) (red    200))
     ((banana X) (yellow 200)))
    |}];
  Incr.Var.set m1_var (String.Map.of_alist_exn [ "banana", "yellow"; "grape", "purple" ]);
  stabilize_and_show ();
  [%expect
    {|
    (((banana X) (yellow 200))
     ((grape  X) (purple 200)))
    |}];
  Incr.Var.set m2_var (Char.Map.of_alist_exn [ 'Y', 400; 'Z', 500 ]);
  stabilize_and_show ();
  [%expect
    {|
    (((banana Y) (yellow 400))
     ((banana Z) (yellow 500))
     ((grape  Y) (purple 400))
     ((grape  Z) (purple 500)))
    |}];
  Incr.Var.set m2_var (Char.Map.of_alist_exn [ 'Y', 400; 'Z', 600 ]);
  stabilize_and_show ();
  [%expect
    {|
    (((banana Y) (yellow 400))
     ((banana Z) (yellow 600))
     ((grape  Y) (purple 400))
     ((grape  Z) (purple 600)))
    |}];
  Incr.Var.set m1_var String.Map.empty;
  stabilize_and_show ();
  [%expect {| () |}];
  Incr.Var.set m1_var (String.Map.singleton "grape" "purple");
  stabilize_and_show ();
  [%expect
    {|
    (((grape Y) (purple 400))
     ((grape Z) (purple 600)))
    |}]
;;

let%test_unit "randomly generated inputs for cartesian product should produce correct \
               outputs"
  =
  let m1_var = Incr.Var.create Int.Map.empty in
  let m2_var = Incr.Var.create Int.Map.empty in
  let observer =
    Incr.observe
      (Incr_map.cartesian_product
         (Incr.Var.watch m1_var)
         (Incr.Var.watch m2_var)
         ~data_equal_left:equal
         ~data_equal_right:equal)
  in
  let all_at_once_impl m1 m2 =
    List.cartesian_product (Map.to_alist m1) (Map.to_alist m2)
    |> List.map ~f:(fun ((k1, v1), (k2, v2)) -> (k1, k2), (v1, v2))
    |> Map.of_alist_exn (module Tuple.Comparator (Int) (Int))
  in
  Quickcheck.test
    (Quickcheck.Generator.tuple2
       (Int.Map.quickcheck_generator
          Quickcheck.Generator.small_non_negative_int
          Quickcheck.Generator.small_non_negative_int)
       (Int.Map.quickcheck_generator
          Quickcheck.Generator.small_non_negative_int
          Quickcheck.Generator.small_non_negative_int))
    ~f:(fun (m1, m2) ->
      Incr.Var.set m1_var m1;
      Incr.Var.set m2_var m2;
      Incr.stabilize ();
      [%test_result: ((int * int) * (int * int)) list]
        ~expect:(all_at_once_impl m1 m2 |> Map.to_alist)
        (Incr.Observer.value_exn observer |> Map.to_alist))
;;
