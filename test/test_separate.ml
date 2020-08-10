open Core_kernel
open Import

let%expect_test "separate -> join" =
  let input_original_map = Incr.Var.create String.Map.empty in
  let original_map = Incr.observe (Incr.Var.watch input_original_map) in
  let separated_map =
    Incr.observe
      (Incr.Map.separate (Incr.Observer.observing original_map) ~data_equal:Int.equal)
  in
  Incr.Observer.on_update_exn separated_map ~f:(fun change ->
    let old_, new_ =
      match change with
      | Invalidated -> failwith "Should not be invalidated"
      | Initialized new_ -> String.Map.empty, new_
      | Changed (old_, new_) -> old_, new_
    in
    Map.symmetric_diff old_ new_ ~data_equal:phys_equal
    |> Sequence.iter ~f:(fun (key, change) ->
      match change with
      | `Left _ -> print_s [%message "removed_node" ~_:(key : string)]
      | `Right _ -> print_s [%message "added_node" ~_:(key : string)]
      | `Unequal _ -> failwith "Should not remake a node for a key"));
  let rejoined_map =
    Incr.observe (Incr.Map.join (Incr.Observer.observing separated_map))
  in
  Incr.Observer.on_update_exn rejoined_map ~f:(fun change ->
    let old_, new_ =
      match change with
      | Invalidated -> failwith "Should not be invalidated"
      | Initialized new_ -> String.Map.empty, new_
      | Changed (old_, new_) -> old_, new_
    in
    print_s [%message "current_map" ~_:(new_ : int String.Map.t)];
    Map.symmetric_diff old_ new_ ~data_equal:Int.equal
    |> Sequence.iter ~f:(fun (key, change) ->
      match change with
      | `Left x -> print_s [%message "removed" (key : string) ~_:(x : int)]
      | `Right x -> print_s [%message "added" (key : string) ~_:(x : int)]
      | `Unequal (from, into) ->
        print_s [%message "changed" (key : string) (from : int) (into : int)]));
  let run_test alist =
    Incr.Var.set input_original_map (String.Map.of_alist_exn alist);
    Incr.stabilize ();
    let original_map = Incr.Observer.value_exn original_map
    and rejoined_map = Incr.Observer.value_exn rejoined_map in
    require
      [%here]
      ([%compare.equal: int String.Map.t] original_map rejoined_map)
      ~if_false_then_print_s:
        (lazy
          [%message
            "maps_differ"
              (original_map : int String.Map.t)
              (rejoined_map : int String.Map.t)])
  in
  run_test [];
  [%expect {|
    (current_map ()) |}];
  run_test [ "a", 1 ];
  [%expect {|
    (added_node a)
    (current_map ((a 1)))
    (added (key a) 1) |}];
  run_test [ "a", 2 ];
  [%expect
    {|
    (current_map ((a 2)))
    (changed
      (key  a)
      (from 1)
      (into 2)) |}];
  run_test [ "b", 3 ];
  [%expect
    {|
    (removed_node a)
    (added_node b)
    (current_map ((b 3)))
    (removed (key a) 2)
    (added (key b) 3) |}];
  run_test [ "b", 4 ];
  [%expect
    {|
    (current_map ((b 4)))
    (changed
      (key  b)
      (from 3)
      (into 4)) |}];
  run_test [ "a", 1; "b", 2 ];
  [%expect
    {|
    (added_node a)
    (current_map (
      (a 1)
      (b 2)))
    (added (key a) 1)
    (changed
      (key  b)
      (from 4)
      (into 2)) |}];
  run_test [ "a", 10; "b", 20 ];
  [%expect
    {|
    (current_map (
      (a 10)
      (b 20)))
    (changed
      (key  a)
      (from 1)
      (into 10))
    (changed
      (key  b)
      (from 2)
      (into 20)) |}];
  run_test [ "a", 100; "b", 200 ];
  [%expect
    {|
    (current_map (
      (a 100)
      (b 200)))
    (changed
      (key  a)
      (from 10)
      (into 100))
    (changed
      (key  b)
      (from 20)
      (into 200)) |}];
  run_test [ "a", 100 ];
  [%expect
    {|
    (removed_node b)
    (current_map ((a 100)))
    (removed (key b) 200) |}];
  run_test [ "a", 100; "b", 3 ];
  [%expect
    {|
    (added_node b)
    (current_map (
      (a 100)
      (b 3)))
    (added (key b) 3) |}]
;;

let%expect_test "separate -> join (but random)" =
  let input_original_map =
    Incr.Var.create (Rand_map_helper.init_rand_map ~from:0 ~to_:1000)
  in
  let original_map = Incr.observe (Incr.Var.watch input_original_map) in
  let rejoined_map =
    Incr.Var.watch input_original_map
    |> Incr.Map.separate ~data_equal:Float.equal
    |> Incr.Map.join
    |> Incr.observe
  in
  List.iter (List.range 0 1000) ~f:(fun _ ->
    let new_map = Rand_map_helper.rand_modify_map (Incr.Var.value input_original_map) in
    Incr.Var.set input_original_map new_map;
    Incr.stabilize ();
    let original_map = Incr.Observer.value_exn original_map
    and rejoined_map = Incr.Observer.value_exn rejoined_map in
    require
      [%here]
      ([%compare.equal: float Int.Map.t] original_map rejoined_map)
      ~if_false_then_print_s:
        (lazy
          [%message
            "maps_differ"
              (original_map : float Int.Map.t)
              (rejoined_map : float Int.Map.t)]))
;;

let%expect_test "test for extra recalculations" =
  let the_var = Incr.Var.create String.Map.empty in
  let known_nodes = ref [] in
  let separated = Incr.Map.separate (Incr.Var.watch the_var) ~data_equal:Int.equal in
  Incr.set_cutoff separated Incr.Cutoff.never;
  let iter_over_map =
    Map.iteri ~f:(fun ~key ~data ->
      if not (List.mem ~equal:phys_equal !known_nodes data)
      then (
        known_nodes := data :: !known_nodes;
        Incr.set_cutoff data Incr.Cutoff.never;
        Incr.Observer.on_update_exn (Incr.observe data) ~f:(function
          | Initialized v -> printf "Key %s initialized to %d\n" key v
          | Changed (old_v, new_v) ->
            printf "Key %s changed to %d (was %d)\n" key new_v old_v
          | Invalidated -> printf "Key %s invalidated\n" key)))
  in
  Incr.Observer.on_update_exn (Incr.observe separated) ~f:(function
    | Initialized initialized -> iter_over_map initialized
    | Changed (_old, new_map) -> iter_over_map new_map
    | Invalidated -> ());
  let run_test alist =
    Incr.Var.set the_var (String.Map.of_alist_exn alist);
    Incr.stabilize ();
    Incr.stabilize ()
  in
  run_test [];
  [%expect {||}];
  run_test [ "a", 1 ];
  [%expect {| Key a initialized to 1 |}];
  run_test [ "a", 2 ];
  [%expect {| Key a changed to 2 (was 1) |}];
  run_test [ "b", 3 ];
  [%expect {|
    Key a invalidated
    Key b initialized to 3 |}];
  run_test [ "b", 4 ];
  [%expect {| Key b changed to 4 (was 3) |}];
  run_test [ "a", 1; "b", 2 ];
  [%expect {|
    Key b changed to 2 (was 4)
    Key a initialized to 1 |}];
  run_test [ "a", 10; "b", 20 ];
  [%expect {|
    Key b changed to 20 (was 2)
    Key a changed to 10 (was 1) |}];
  run_test [ "a", 100; "b", 200 ];
  [%expect {|
    Key b changed to 200 (was 20)
    Key a changed to 100 (was 10) |}];
  run_test [ "a", 100 ];
  [%expect {| Key b invalidated |}];
  run_test [ "a", 100; "b", 3 ];
  [%expect {| Key b initialized to 3 |}]
;;
