open Core_kernel.Std
open Expect_test_helpers_kernel.Std

let%test_module "Basic Incr_map tests" =
  (module struct
    module Incr = Incremental_kernel.Std.Incremental.Make()
    module Incr_map = Incr_map.Make(Incr)

    let%expect_test "check join against slow implementation" =
      let input_shape = Incr.Var.create Int.Map.empty in
      let watch_shape = Incr.Var.watch input_shape in
      let via_incr = Incr.observe (
        Incr_map.join watch_shape)
      and via_map = Incr.observe (
        let open Incr.Let_syntax in
        let%bind shape = watch_shape in
        let%map alist = Incr.all (
          Map.to_sequence shape
          |> Sequence.map ~f:(fun (key, data) ->
            Incr.map data ~f:(Tuple2.create key))
          |> Sequence.to_list_rev)
        in
        Map.of_alist_exn ~comparator:(Map.comparator shape) alist)
      in
      let test_now () =
        Incr.stabilize ();
        print_s [%sexp (Incr.Observer.value_exn via_map : string Int.Map.t)];
        [%test_result: string Int.Map.t]
          ~expect:(Incr.Observer.value_exn via_map)
          (Incr.Observer.value_exn via_incr)
      and set_map alist =
        Incr.Var.set input_shape (Int.Map.of_alist_exn alist)
      in

      (* This tests for the empty map initialisation problem. *)
      test_now ();
      [%expect {| () |}];
      (* Some other tests manually messing around with the vars. *)
      let one_var = Incr.Var.create "one" in
      let one_incr = Incr.Var.watch one_var in
      set_map [1, one_incr];
      test_now ();
      [%expect {|
        ((1 one)) |}];
      let one_incr =
        let new_one_incr = Incr.map ~f:Fn.id one_incr in
        assert (not (phys_same one_incr new_one_incr));
        assert (not (phys_equal one_incr new_one_incr));
        new_one_incr
      in
      set_map [1, one_incr];
      test_now ();
      [%expect {| ((1 one)) |}];
      Incr.Var.set one_var "two";
      test_now ();
      [%expect {| ((1 two)) |}];
      let two_var = Incr.Var.create "two" in
      let two_incr = Incr.Var.watch two_var in
      Incr.Var.set one_var "one";
      set_map [1, one_incr; 2, two_incr];
      test_now ();
      [%expect {|
        ((1 one)
         (2 two)) |}];
      let test_with =
        let vars = ref Int.Map.empty
        and incrs = ref Int.Map.empty
        and old_map = ref Int.Map.empty
        in
        fun alist ->
          let map = Int.Map.of_alist_exn alist in
          Map.symmetric_diff !old_map map ~data_equal:String.equal
          |> Sequence.iter ~f:(fun (key, change) ->
            match change with
            | `Left _ ->
              vars := Map.remove !vars key;
              incrs := Map.remove !incrs key;
            | `Right new_value ->
              let new_var = Incr.Var.create new_value in
              vars := Map.add !vars ~key ~data:new_var;
              incrs := Map.add !incrs ~key ~data:(Incr.Var.watch new_var);
            | `Unequal (_, new_value) ->
              Incr.Var.set (Map.find_exn !vars key) new_value);
          old_map := map;
          Incr.Var.set input_shape !incrs;
          test_now ();
      in
      test_with [1, "one"];
      [%expect {| ((1 one)) |}];
      test_with [1, "two"; 3, "three"];
      [%expect {|
        ((1 two)
         (3 three)) |}];
      test_with [1, "one"; 2, "two"];
      [%expect {|
        ((1 one)
         (2 two)) |}];
      test_with [1, "five"; 3, "three"; 4, "four"];
      [%expect {|
        ((1 five)
         (3 three)
         (4 four)) |}];
      test_with [];
      [%expect {| () |}];
      test_with [1, "five"; 3, "three"; 4, "four"];
      [%expect {|
        ((1 five)
         (3 three)
         (4 four)) |}];
      test_with [1, "one"; 2, "two"];
      [%expect {|
        ((1 one)
         (2 two)) |}];
    ;;
  end)
