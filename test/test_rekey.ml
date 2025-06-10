open! Core
open! Import

let%test_unit "rekey random test" =
  Quickcheck.test
    ~sexp_of:[%sexp_of: (int, int) Map_operations.t list]
    (Map_operations.quickcheck_generator Int.quickcheck_generator)
    ~f:(fun operations ->
      let m = Incr.Var.create Int.Map.empty in
      let watch_m = Incr.Var.watch m in
      let fast =
        Incr_map.rekey ~comparator:(module String) watch_m ~f:(fun ~key ~data:_ ->
          Int.to_string key)
      in
      let slow =
        let%map watch_m in
        watch_m
        |> Map.to_alist
        |> List.map ~f:(fun (k, v) -> Int.to_string k, v)
        |> Map.of_alist_exn (module String)
      in
      let fast_obs = Incr.observe fast in
      let slow_obs = Incr.observe slow in
      Map_operations.run_operations operations ~into:m ~after_stabilize:(fun () ->
        [%test_result: int String.Map.t]
          ~expect:(Incr.Observer.value_exn fast_obs)
          (Incr.Observer.value_exn slow_obs)))
;;

let%expect_test "rekey should order adds and removes properly" =
  (* Rekey relies on the callback function not mapping two keys in the map to
     the same value. However, it should have no problem shrinking the key
     space, though this is less easily achieved. What could happen (and what
     this test checks for) is that at the same time one key is added and a
     different key removed, but both keys are mapped into the same final key.
     This should be legal, but it is easy for a naive implementation to
     accidentally add the element first, and then remove it immediately
     afterward (or, if it used [add_exn], it might raise before even removing). *)
  let m = Incr.Var.create Int.Map.empty in
  let watch_m = Incr.Var.watch m in
  let result =
    Incr_map.rekey ~comparator:(module Int) watch_m ~f:(fun ~key ~data:_ -> key mod 3)
  in
  let result_obs = Incr.observe result in
  let set_to alist = Incr.Var.set m (Int.Map.of_alist_exn alist) in
  let stabilize_and_print () =
    Incr.stabilize ();
    print_s [%sexp (Incr.Observer.value_exn result_obs : int Int.Map.t)]
  in
  stabilize_and_print ();
  [%expect {| () |}];
  set_to [ 3, 0; 4, 1; 5, 2 ];
  stabilize_and_print ();
  [%expect
    {|
    ((0 0)
     (1 1)
     (2 2))
    |}];
  set_to [ 0, 0; 1, 1; 2, 2 ];
  stabilize_and_print ();
  [%expect
    {|
    ((0 0)
     (1 1)
     (2 2))
    |}]
;;

let%expect_test "rekey should order adds and removes properly (part 2)" =
  (* This test checks the situation in which an entry's value is updated such
     that the entry's key is mapped to a removed key. *)
  let m = Incr.Var.create Int.Map.empty in
  let watch_m = Incr.Var.watch m in
  let result =
    Incr_map.rekey ~comparator:(module Int) watch_m ~f:(fun ~key ~data ->
      (key mod 3) + data)
  in
  let result_obs = Incr.observe result in
  let set_to alist = Incr.Var.set m (Int.Map.of_alist_exn alist) in
  let stabilize_and_print () =
    Incr.stabilize ();
    print_s [%sexp (Incr.Observer.value_exn result_obs : int Int.Map.t)]
  in
  stabilize_and_print ();
  [%expect {| () |}];
  set_to [ 3, 0; 4, 0; 5, 0 ];
  stabilize_and_print ();
  [%expect
    {|
    ((0 0)
     (1 0)
     (2 0))
    |}];
  set_to [ 3, 1; 5, 0 ];
  stabilize_and_print ();
  [%expect
    {|
    ((1 1)
     (2 0))
    |}]
;;
