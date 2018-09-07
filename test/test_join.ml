open Core_kernel
open Import

let%expect_test "check join against slow implementation" =
  let input_shape = Incr.Var.create Int.Map.empty in
  let watch_shape = Incr.Var.watch input_shape in
  let via_incr = Incr.observe (Incr.Map.join watch_shape)
  and via_map =
    Incr.observe
      (let open Incr.Let_syntax in
       let%bind shape = watch_shape in
       let%map alist =
         Incr.all
           (Map.to_sequence shape
            |> Sequence.map ~f:(fun (key, data) -> Incr.map data ~f:(Tuple2.create key))
            |> Sequence.to_list_rev)
       in
       Map.Using_comparator.of_alist_exn ~comparator:(Map.comparator shape) alist)
  in
  let test_now () =
    Incr.stabilize ();
    print_s [%sexp (Incr.Observer.value_exn via_map : string Int.Map.t)];
    [%test_result: string Int.Map.t]
      ~expect:(Incr.Observer.value_exn via_map)
      (Incr.Observer.value_exn via_incr)
  and set_map alist = Incr.Var.set input_shape (Int.Map.of_alist_exn alist) in
  (* This tests for the empty map initialisation problem. *)
  test_now ();
  [%expect {| () |}];
  (* Some other tests manually messing around with the vars. *)
  let one_var = Incr.Var.create "one" in
  let one_incr = Incr.Var.watch one_var in
  set_map [ 1, one_incr ];
  test_now ();
  [%expect {|
        ((1 one)) |}];
  let one_incr =
    let new_one_incr = Incr.map ~f:Fn.id one_incr in
    assert (not (phys_same one_incr new_one_incr));
    assert (not (phys_equal one_incr new_one_incr));
    new_one_incr
  in
  set_map [ 1, one_incr ];
  test_now ();
  [%expect {| ((1 one)) |}];
  Incr.Var.set one_var "two";
  test_now ();
  [%expect {| ((1 two)) |}];
  let two_var = Incr.Var.create "two" in
  let two_incr = Incr.Var.watch two_var in
  Incr.Var.set one_var "one";
  set_map [ 1, one_incr; 2, two_incr ];
  test_now ();
  [%expect {|
        ((1 one)
         (2 two)) |}];
  let test_with =
    let vars = ref Int.Map.empty
    and incrs = ref Int.Map.empty
    and old_map = ref Int.Map.empty in
    fun alist ->
      let map = Int.Map.of_alist_exn alist in
      Map.symmetric_diff !old_map map ~data_equal:String.equal
      |> Sequence.iter ~f:(fun (key, change) ->
        match change with
        | `Left _ ->
          vars := Map.remove !vars key;
          incrs := Map.remove !incrs key
        | `Right new_value ->
          let new_var = Incr.Var.create new_value in
          vars := Map.set !vars ~key ~data:new_var;
          incrs := Map.set !incrs ~key ~data:(Incr.Var.watch new_var)
        | `Unequal (_, new_value) -> Incr.Var.set (Map.find_exn !vars key) new_value);
      old_map := map;
      Incr.Var.set input_shape !incrs;
      test_now ()
  in
  test_with [ 1, "one" ];
  [%expect {| ((1 one)) |}];
  test_with [ 1, "two"; 3, "three" ];
  [%expect {|
        ((1 two)
         (3 three)) |}];
  test_with [ 1, "one"; 2, "two" ];
  [%expect {|
        ((1 one)
         (2 two)) |}];
  test_with [ 1, "five"; 3, "three"; 4, "four" ];
  [%expect {|
        ((1 five)
         (3 three)
         (4 four)) |}];
  test_with [];
  [%expect {| () |}];
  test_with [ 1, "five"; 3, "three"; 4, "four" ];
  [%expect {|
        ((1 five)
         (3 three)
         (4 four)) |}];
  test_with [ 1, "one"; 2, "two" ];
  [%expect {|
        ((1 one)
         (2 two)) |}]
;;

let%test_module "random tests" =
  (module struct
    (* [Incr.Map.join] is tested as follows:

       First, create [map_of_incrs_incr] of type [float Incr.t Int.Map.t Incr.t] with
       initial values equal to those in [map].

       Next, apply [Incr.Map.join] to [map_of_incrs_incr] to get [result_incr].

       At each of the [num_steps] steps, randomly change the value of [map_of_incrs] in
       one of two ways:
       - add, remove, or replace a single entry in the map
       - set the [Incr.Var.t] corresponding to the data of a single entry to a new value

       Every [stabilize_every_n] steps, check the result as follows:
       - call [Incr.stabilize ()]
       - check the value of [result_incr]
    *)
    let test_join map ~steps ~stabilize_every_n =
      let map_of_vars_var = Incr.Var.create (Map.map map ~f:Incr.Var.create) in
      let map_of_vars_incr = Incr.Var.watch map_of_vars_var in
      let map_of_incrs_incr = Incr.map map_of_vars_incr ~f:(Map.map ~f:Incr.Var.watch) in
      let result_incr = Incr.Map.join map_of_incrs_incr in
      let result_obs = Incr.observe result_incr in
      (* Since [result_incr] was obtained as [Incr.Map.join map_of_incrs_incr], check the
         value of [result_incr] against the data values in [map_of_incrs_incr] *)
      let test_value () =
        Incr.stabilize ();
        [%test_result: float Int.Map.t]
          (Incr.Observer.value_exn result_obs)
          ~expect:(Map.map (Incr.Var.value map_of_vars_var) ~f:Incr.Var.value)
      in
      let stabilize_and_test_result () =
        Incr.stabilize ();
        test_value ()
      in
      stabilize_and_test_result ();
      List.iter (List.range 0 steps) ~f:(fun i ->
        let map_of_vars =
          Rand_map_helper.rand_modify_map_of_vars (Incr.Var.value map_of_vars_var)
        in
        if i % stabilize_every_n = 0
        then (
          Incr.Var.set map_of_vars_var map_of_vars;
          stabilize_and_test_result ()))
    ;;

    let%test_unit "rand test: start with empty map, stabilize every step" =
      test_join Int.Map.empty ~steps:100 ~stabilize_every_n:1
    ;;

    let%test_unit "rand test: start with non-empty map, stabilize every step" =
      let start_map = Rand_map_helper.init_rand_map ~from:0 ~to_:30 in
      test_join start_map ~steps:100 ~stabilize_every_n:1
    ;;

    let%test_unit "rand test: start with empty map, stabilize every 10 steps" =
      test_join Int.Map.empty ~steps:100 ~stabilize_every_n:10
    ;;
  end)
;;
