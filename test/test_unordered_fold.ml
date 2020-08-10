open! Core
open! Import

(* version of unorderd fold which will fail if there isn't a match. *)
let unordered_fold (type a) ~data_equal ?specialized_initial m ~(init : a) ~add ~remove =
  let a = Incr.Map.unordered_fold ~data_equal ?specialized_initial m ~init ~add ~remove in
  let b =
    let%map m = m in
    Map.fold ~init ~f:add m
  in
  let%map a = a
  and b = b in
  require [%here] (data_equal a b);
  a
;;

let%expect_test _ =
  let map = Incr.Var.create (String.Map.of_alist_exn [ "a", 1; "b", 2 ]) in
  let map_o = Incr.observe (Incr.Var.watch map) in
  let sum_o =
    unordered_fold
      (Incr.Var.watch map)
      ~data_equal:Int.equal
      ~init:0
      ~add:(fun ~key:_ ~data:v acc -> acc + v)
      ~remove:(fun ~key:_ ~data:v acc -> acc - v)
    |> Incr.observe
  in
  let dump () =
    Incr.stabilize ();
    let value = Incr.Observer.value_exn in
    Sexp.to_string_hum ([%sexp_of: int * int String.Map.t] (value sum_o, value map_o))
    |> print_endline
  in
  let change f = Incr.Var.set map (f (Incr.Var.value map)) in
  dump ();
  [%expect {| (3 ((a 1) (b 2))) |}];
  change (fun m -> Map.set m ~key:"c" ~data:4);
  dump ();
  [%expect {| (7 ((a 1) (b 2) (c 4))) |}];
  change (fun m -> Map.remove m "b");
  dump ();
  [%expect {| (5 ((a 1) (c 4))) |}];
  change (fun m -> Map.set m ~key:"c" ~data:0);
  dump ();
  [%expect {| (1 ((a 1) (c 0))) |}]
;;

let%expect_test _ =
  let map = Incr.Var.create (String.Map.of_alist_exn [ "a", 1; "b", 2 ]) in
  let map_o = Incr.observe (Incr.Var.watch map) in
  let sum_o =
    unordered_fold
      (Incr.Var.watch map)
      ~data_equal:Int.equal
      ~init:0
      ~add:(fun ~key:_ ~data:v acc -> acc + v)
      ~remove:(fun ~key:_ ~data:v acc -> acc - v)
      ~specialized_initial:(fun ~init:_ map ->
        Core.List.sum (module Int) ~f:Fn.id (Map.data map))
    |> Incr.observe
  in
  let dump () =
    Incr.stabilize ();
    let value = Incr.Observer.value_exn in
    Sexp.to_string_hum ([%sexp_of: int * int String.Map.t] (value sum_o, value map_o))
    |> print_endline
  in
  dump ();
  [%expect {| (3 ((a 1) (b 2))) |}]
;;

let%test_module "random tests" =
  (module struct
    (* [add_count], [remove_count] and [update_count] are counters used to keep track of
       how many times [add], [remove] and [update] respectively are called by
       [Incr.Map.unordered_fold]. *)
    let add_count = ref 0
    let remove_count = ref 0
    let update_count = ref 0

    (* Symbolic [add], [remove] satisfying the minimal required properties:
       - [remove ~key ~data] is the inverse of [add ~key ~data]
       - [add ~key ~data] and [add ~key:key' ~data:data'] can be commuted if [key <> key']
       - [update ~key ~old_data ~new_data] is the (possibly simplified) composition of
         [remove ~key ~data:old_data] and [add ~key ~data:new_data]
    *)
    let init = Int.Map.empty

    type 'a op =
      | Add of 'a
      | Remove of 'a
      | Update of 'a * 'a
    [@@deriving sexp, compare]

    let add ~key ~data map = Map.add_multi map ~key ~data:(Add data)

    let remove ~key ~data map =
      Map.change map key ~f:(function
        | None -> Some [ Remove data ]
        | Some (Add other_data :: other_ops) when Float.equal data other_data ->
          Option.some_if (not (List.is_empty other_ops)) other_ops
        | Some other_ops -> Some (Remove data :: other_ops))
    ;;

    let update ~key ~old_data ~new_data map =
      Map.change map key ~f:(function
        | None -> Some [ Update (old_data, new_data) ]
        | Some (Add other_data :: other_ops) when Float.equal old_data other_data ->
          Some (Add new_data :: other_ops)
        | Some other_ops -> Some (Update (old_data, new_data) :: other_ops))
    ;;

    (* [incr_map_fold] is the [Incr.Map] function being tested *)
    let incr_map_fold map ~use_update =
      let update =
        Option.some_if use_update (fun ~key ~old_data ~new_data acc ->
          incr update_count;
          update ~key ~old_data ~new_data acc)
      in
      Incr.Map.unordered_fold
        map
        ?update
        ~init
        ~add:(fun ~key ~data acc ->
          incr add_count;
          add ~key ~data acc)
        ~remove:(fun ~key ~data acc ->
          incr remove_count;
          remove ~key ~data acc)
    ;;

    (* [map_fold] is the equivalent [Map] function that we test against *)
    let map_fold map = Map.fold map ~init ~f:add

    (* [Incr.Map.unordered_fold] is tested as follows:

       First, create [map_incr] of type [float Int.Map.t Incr.t] with initial value [map].

       Next, apply [incr_map_fold] to [map_incr] to get [result_incr].

       At each of the [num_steps] steps, randomly change the value of [map_incr]
       by adding, removing, or replacing a single entry.

       Every [stabilize_every_n] steps, check the result as follows:
       - reset the counters to 0
       - call [Incr.stabilize ()]
       - check the value of [result_incr]
       - check the counter values
    *)
    let test_unordered_fold map ~steps ~stabilize_every_n ~use_update =
      let map_var = Incr.Var.create map in
      let map_incr = Incr.Var.watch map_var in
      let map_obs = Incr.observe map_incr in
      let result_incr = incr_map_fold map_incr ~use_update in
      let result_obs = Incr.observe result_incr in
      let reset_counters () =
        add_count := 0;
        remove_count := 0;
        update_count := 0
      in
      let test_value () =
        (* Since [result_incr] was obtained as [incr_map_fold map_incr], check that the
           value of [result_incr] is equal to the result of applying the equivalent
           function [map_fold] directly to the value of [map_incr] *)
        let expected_result = map_fold (Incr.Observer.value_exn map_obs) in
        let actual_result = Incr.Observer.value_exn result_obs in
        [%test_result: float op list Int.Map.t] actual_result ~expect:expected_result
      in
      let test_counters ~old_map ~new_map =
        (* It is expected that:
           - each time an entry is removed, [remove] is called once
           - each time an entry is added, [add] is called once
           - each time an entry is replaced:
           - if [use_update = true], [update] is called once
           - if [use_update = false], [add] and [remove] are each called once
        *)
        let expect =
          let symmetric_diff =
            Map.symmetric_diff old_map new_map ~data_equal:phys_equal
          in
          Sequence.fold
            symmetric_diff
            ~init:(0, 0, 0)
            ~f:(fun (add_count, remove_count, update_count) (_, diff) ->
              match diff with
              | `Left _ -> add_count, remove_count + 1, update_count
              | `Right _ -> add_count + 1, remove_count, update_count
              | `Unequal _ ->
                if use_update
                then add_count, remove_count, update_count + 1
                else add_count + 1, remove_count + 1, update_count)
        in
        [%test_result: int * int * int] ~expect (!add_count, !remove_count, !update_count)
      in
      let stabilize_and_test_result ~old_map ~new_map =
        reset_counters ();
        Incr.stabilize ();
        test_value ();
        test_counters ~old_map ~new_map
      in
      stabilize_and_test_result ~old_map:Int.Map.empty ~new_map:map;
      let old_map = ref map in
      List.fold (List.range 0 steps) ~init:map ~f:(fun map i ->
        let map = Rand_map_helper.rand_modify_map map in
        if i % stabilize_every_n = 0
        then (
          Incr.Var.set map_var map;
          stabilize_and_test_result ~old_map:!old_map ~new_map:map;
          old_map := map);
        map)
      |> fun _map -> ()
    ;;

    let%test_unit "rand test: start with empty map, stabilize every step, no update" =
      test_unordered_fold Int.Map.empty ~steps:100 ~stabilize_every_n:1 ~use_update:false
    ;;

    let%test_unit "rand test: start with non-empty map, stabilize every step, no update" =
      let start_map = Rand_map_helper.init_rand_map ~from:0 ~to_:30 in
      test_unordered_fold start_map ~steps:100 ~stabilize_every_n:1 ~use_update:false
    ;;

    let%test_unit "rand test: start with empty map, stabilize every 10 steps, no update" =
      test_unordered_fold Int.Map.empty ~steps:100 ~stabilize_every_n:10 ~use_update:false
    ;;

    let%test_unit "rand test: start with empty map, stabilize every step, update" =
      test_unordered_fold Int.Map.empty ~steps:100 ~stabilize_every_n:1 ~use_update:true
    ;;

    let%test_unit "rand test: start with non-empty map, stabilize every step, update" =
      let start_map = Rand_map_helper.init_rand_map ~from:0 ~to_:30 in
      test_unordered_fold start_map ~steps:100 ~stabilize_every_n:1 ~use_update:true
    ;;

    let%test_unit "rand test: start with empty map, stabilize every 10 steps, update" =
      test_unordered_fold Int.Map.empty ~steps:100 ~stabilize_every_n:10 ~use_update:true
    ;;
  end)
;;
