open Core
open Import

let print_lookup_state lookup =
  print_s ([%sexp_of: (int, string) Incr.Map.Lookup.For_debug.t] lookup)
;;

let%expect_test "test unnecessary nodes are cleaned" =
  let input_map =
    Incr.Var.create
      (Int.Map.of_alist_exn [ 1, "hello"; 2, "world"; 4, "abc"; 7, "def"; 9, "xyz" ])
  in
  let lookup =
    Incr.Map.Lookup.create
      (module Int)
      (Incr.Var.watch input_map)
      ~data_equal:String.equal
  in
  let key_observers =
    match
      Int.Table.create_mapped (List.range 0 10) ~get_key:Fn.id ~get_data:(fun key ->
        Incr.observe (Incr.Map.Lookup.find lookup key))
    with
    | `Duplicate_keys _ -> failwith "Hit impossible case"
    | `Ok table -> table
  in
  ignore (Incr.Map.Lookup.find lookup 10);
  print_lookup_state lookup;
  [%expect {| () |}];
  Incr.stabilize ();
  print_lookup_state lookup;
  [%expect
    {|
    (((key 0) (entries (((saved_value ())))))
     ((key          1)
      (actual_value hello)
      (entries (((saved_value (hello))))))
     ((key          2)
      (actual_value world)
      (entries (((saved_value (world))))))
     ((key 3) (entries (((saved_value ())))))
     ((key          4)
      (actual_value abc)
      (entries (((saved_value (abc))))))
     ((key 5) (entries (((saved_value ())))))
     ((key 6) (entries (((saved_value ())))))
     ((key          7)
      (actual_value def)
      (entries (((saved_value (def))))))
     ((key 8) (entries (((saved_value ())))))
     ((key          9)
      (actual_value xyz)
      (entries (((saved_value (xyz)))))))
    |}];
  (* Stop looking at some keys. Initially they are still live. *)
  List.iter [ 1; 4; 5 ] ~f:(fun key ->
    Hashtbl.find_and_remove key_observers key
    |> Option.value_exn
    |> Incr.Observer.disallow_future_use);
  print_lookup_state lookup;
  [%expect
    {|
    (((key 0) (entries (((saved_value ())))))
     ((key          1)
      (actual_value hello)
      (entries (((saved_value (hello))))))
     ((key          2)
      (actual_value world)
      (entries (((saved_value (world))))))
     ((key 3) (entries (((saved_value ())))))
     ((key          4)
      (actual_value abc)
      (entries (((saved_value (abc))))))
     ((key 5) (entries (((saved_value ())))))
     ((key 6) (entries (((saved_value ())))))
     ((key          7)
      (actual_value def)
      (entries (((saved_value (def))))))
     ((key 8) (entries (((saved_value ())))))
     ((key          9)
      (actual_value xyz)
      (entries (((saved_value (xyz)))))))
    |}];
  Incr.stabilize ();
  print_lookup_state lookup;
  [%expect
    {|
    (((key 0) (entries (((saved_value ())))))
     ((key          1)
      (actual_value hello)
      (entries ()))
     ((key          2)
      (actual_value world)
      (entries (((saved_value (world))))))
     ((key 3) (entries (((saved_value ())))))
     ((key          4)
      (actual_value abc)
      (entries ()))
     ((key 6) (entries (((saved_value ())))))
     ((key          7)
      (actual_value def)
      (entries (((saved_value (def))))))
     ((key 8) (entries (((saved_value ())))))
     ((key          9)
      (actual_value xyz)
      (entries (((saved_value (xyz)))))))
    |}]
;;

let%expect_test "test cleaned nodes still work" =
  let input_map = Incr.Var.create (Int.Map.of_alist_exn [ 1, "hello" ]) in
  let lookup =
    Incr.Map.Lookup.create
      (module Int)
      (Incr.Var.watch input_map)
      ~data_equal:String.equal
  in
  let find_a = Incr.Map.Lookup.find lookup 1 in
  Incr.stabilize ();
  [%test_result: bool] ~expect:false (Incr.is_necessary find_a);
  Incr.stabilize ();
  print_lookup_state lookup;
  [%expect {| () |}];
  let find_b = Incr.Map.Lookup.find lookup 1 in
  let obs_a = Incr.observe find_a
  and obs_b = Incr.observe find_b in
  Incr.Var.set input_map (Int.Map.of_alist_exn [ 1, "world" ]);
  Incr.stabilize ();
  print_lookup_state lookup;
  [%expect
    {|
    ((
      (key          1)
      (actual_value world)
      (entries (
        ((saved_value (world)))
        ((saved_value (world)))))))
    |}];
  show_raise (fun () ->
    [%test_result: string option]
      ~expect:(Incr.Observer.value_exn obs_b)
      (Incr.Observer.value_exn obs_a);
    [%test_result: string option] ~expect:(Some "world") (Incr.Observer.value_exn obs_a));
  [%expect {| "did not raise" |}];
  Incr.Observer.disallow_future_use obs_b;
  Incr.stabilize ();
  (* To unlink b. *)
  print_lookup_state lookup;
  [%expect
    {|
    ((
      (key          1)
      (actual_value world)
      (entries (((saved_value (world)))))))
    |}];
  let obs_b = Incr.observe find_b in
  Incr.Var.set input_map (Int.Map.of_alist_exn [ 1, "and others" ]);
  Incr.stabilize ();
  print_lookup_state lookup;
  [%expect
    {|
    ((
      (key          1)
      (actual_value "and others")
      (entries (
        ((saved_value ("and others")))
        ((saved_value ("and others")))))))
    |}];
  show_raise (fun () ->
    [%test_result: string option]
      ~expect:(Incr.Observer.value_exn obs_b)
      (Incr.Observer.value_exn obs_a);
    [%test_result: string option]
      ~expect:(Some "and others")
      (Incr.Observer.value_exn obs_a));
  [%expect {| "did not raise" |}]
;;

let%test_unit "test vs slow lookup" =
  let input_map = Incr.Var.create Int.Map.empty in
  let lookup =
    Incr.Map.Lookup.create
      (module Int)
      (Incr.Var.watch input_map)
      ~data_equal:String.equal
  in
  let find_nodes =
    List.(map (range 0 10)) ~f:(fun key ->
      let with_lookup = Incr.observe (Incr.Map.Lookup.find lookup key)
      and slow_path =
        Incr.observe (Incr.map ~f:(fun m -> Map.find m key) (Incr.Var.watch input_map))
      in
      key, (with_lookup, slow_path))
    |> Int.Map.of_alist_exn
  in
  let test_alist alist =
    Incr.Var.set input_map (Int.Map.of_alist_exn alist);
    Incr.stabilize ();
    Map.iteri find_nodes ~f:(fun ~key ~data:(with_lookup, slow_path) ->
      [%test_result: string option]
        ~message:(sprintf "matches slow path for key %i" key)
        ~expect:(Incr.Observer.value_exn slow_path)
        (Incr.Observer.value_exn with_lookup))
  in
  test_alist [ 1, "hello"; 15, "what"; 3, "bear" ];
  test_alist [ 5, "hello"; 7, "what"; 1, "bear" ];
  test_alist [ 1, "hello"; 7, "nine"; 12, "bear" ];
  test_alist [ 1, "hello"; 7, "furnish"; 3, "bear" ];
  test_alist [ 1, "hello"; 15, "what"; 3, "bear" ];
  test_alist [ 1, "hello"; 15, "what" ];
  test_alist [ 1, "hello"; 15, "what"; 5, "five"; 7, "seven"; 2, "two" ]
;;

(* This bug was related to nodes which become necessary after being unnecessary while
   the related value in the map was changed. *)
let%expect_test "double lookup bug has been fixed" =
  let map_var = Incr.Var.create String.Map.empty in
  let key = "A" in
  Incr.Var.set map_var (Map.add_exn (Incr.Var.latest_value map_var) ~key ~data:1);
  let lookup = Incr_map.Lookup.create (module String) (Incr.Var.watch map_var) in
  let incr1 = Incr_map.Lookup.find lookup key in
  let incr2 = Incr_map.Lookup.find lookup key in
  let a = Incr.observe incr1 in
  Incr.stabilize ();
  let b = Incr.observe incr2 in
  Incr.stabilize ();
  let a = Incr.Observer.value_exn a
  and b = Incr.Observer.value_exn b in
  require ([%compare.equal: int option] a b);
  [%expect {| |}]
;;

let%expect_test _ =
  let map_var = Incr.Var.create String.Map.empty in
  let key = "A" in
  Incr.Var.set map_var (Map.add_exn (Incr.Var.latest_value map_var) ~key ~data:1);
  let lookup = Incr_map.Lookup.create (module String) (Incr.Var.watch map_var) in
  let a = Incr_map.Lookup.find lookup key in
  (* depend on a *)
  let a_obs = Incr.observe a in
  Incr.stabilize ();
  (* remove dependency on a *)
  Incr.Observer.disallow_future_use a_obs;
  Incr.stabilize ();
  (* depend on b *)
  let b = Incr_map.Lookup.find lookup key in
  let b_obs = Incr.observe b in
  Incr.stabilize ();
  (* remove dependency on a *)
  Incr.Observer.disallow_future_use b_obs;
  Incr.stabilize ();
  (* reintroduce dependency on a *)
  let a_obs = Incr.observe a in
  Incr.stabilize ();
  (* reintroduce dependency on b *)
  let b_obs = Incr.observe b in
  Incr.stabilize ();
  Incr.Var.set map_var (String.Map.singleton "A" 2);
  Incr.stabilize ();
  let a = Incr.Observer.value_exn a_obs
  and b = Incr.Observer.value_exn b_obs in
  require ([%equal: int option] a b)
;;

let%expect_test "re-linking lookup nodes that become necessary after being removed from \
                 the map are correctly recomputed"
  =
  let key_a = "A" in
  let map_var = Incr.Var.create (String.Map.singleton key_a 1) in
  let incr_map_lookup = Incr_map.Lookup.create (module String) (Incr.Var.watch map_var) in
  let direct_lookup key = Map.find (Incr.Var.latest_value map_var) key in
  let _ =
    (* keep the [incr_map_lookup] necessary *)
    Incr.observe (Incr_map.Lookup.find incr_map_lookup "")
  in
  let a = Incr_map.Lookup.find incr_map_lookup key_a in
  let observer_a_1 = Incr.observe a in
  (* observe a and see we receive the true value *)
  Incr.stabilize ();
  require
    ([%equal: int option] (Incr.Observer.value_exn observer_a_1) (direct_lookup key_a));
  [%expect {| |}];
  Incr.Observer.disallow_future_use observer_a_1;
  (* remove the dependency from a *)
  Incr.stabilize ();
  Incr.Var.set map_var String.Map.empty;
  (* clear the value at key_a while a is not observed *)
  Incr.stabilize ();
  let observer_a_2 = Incr.observe a in
  Incr.stabilize ();
  (* re-observe a and correctly receive the new value *)
  require_equal
    (module struct
      type t = int option [@@deriving sexp_of, equal]
    end)
    (Incr.Observer.value_exn observer_a_2)
    (direct_lookup key_a);
  [%expect {| |}]
;;
