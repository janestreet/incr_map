open! Core
open! Import

(* version of unorderd fold which will fail if there isn't a match. *)
let unordered_fold (type a) ~data_equal m ~(init:a) ~f ~f_inverse =
  let a = Incr.Map.unordered_fold ~data_equal m ~init ~f ~f_inverse in
  let b =
    let%map m = m in
    Map.fold ~init ~f m
  in
  let%map a = a and b = b in
  require [%here] (data_equal a b);
  a

let%expect_test _ =
  let map = Incr.Var.create (String.Map.of_alist_exn ["a", 1; "b", 2]) in
  let map_o = Incr.observe (Incr.Var.watch map) in
  let sum_o =
    unordered_fold (Incr.Var.watch map)
      ~data_equal:Int.equal ~init:0
      ~f:(fun ~key:_ ~data:v acc -> acc + v)
      ~f_inverse:(fun ~key:_ ~data:v acc -> acc - v)
    |> Incr.observe
  in
  let dump () =
    Incr.stabilize ();
    let value = Incr.Observer.value_exn in
    Sexp.to_string_hum ([%sexp_of: (int * int String.Map.t)] (value sum_o, value map_o))
    |> print_endline
  in
  let change f = Incr.Var.set map (f (Incr.Var.value map)) in
  dump (); [%expect {| (3 ((a 1) (b 2))) |}];
  change (fun m -> Map.add m ~key:"c" ~data:4);
  dump (); [%expect {| (7 ((a 1) (b 2) (c 4))) |}];
  change (fun m -> Map.remove m "b");
  dump (); [%expect {| (5 ((a 1) (c 4))) |}];
  change (fun m -> Map.add m ~key:"c" ~data:0);
  dump (); [%expect {| (1 ((a 1) (c 0))) |}];
;;

let%test_module "random tests" =
  (module struct

    (* [f_count] and [f_inv_count] are counters used to keep track of how many times [f]
       and [f_inverse] respectively are called by [Incr.Map.unordered_fold]. *)
    let f_count     = ref 0
    let f_inv_count = ref 0

    (* Symbolic [f], [f_inverse] satisfying the minimal required properties,

       i.e. [f_inverse ~key ~data] is the inverse of [f ~key ~data]
       [f ~key ~data] and [f ~key:key' ~data:data'] can be commuted
       if [key <> key'].
    *)
    let init = Int.Map.empty

    let f ~key ~data map = Map.add_multi map ~key ~data:(`F data)

    let f_inverse ~key ~data map = Map.change map key ~f:(function
      | None -> Some [ `F_inverse data ]
      | Some (`F other_data :: other_ops) when Float.equal data other_data ->
        Option.some_if (not (List.is_empty other_ops)) other_ops
      | Some other_ops -> Some (`F_inverse data :: other_ops))

    (* [incr_map_fold] is the [Incr.Map] function being tested *)
    let incr_map_fold map =
      Incr.Map.unordered_fold map
        ~init
        ~f:         (fun ~key ~data acc -> incr f_count; f ~key ~data acc)
        ~f_inverse: (fun ~key ~data acc -> incr f_inv_count; f_inverse ~key ~data acc)
    ;;

    (* [map_fold] is the equivalent [Map] function that we test against *)
    let map_fold map = Map.fold map ~init ~f

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
    let test_unordered_fold map ~steps ~stabilize_every_n =
      let map_var     = Incr.Var.create map      in
      let map_incr    = Incr.Var.watch map_var   in
      let map_obs     = Incr.observe map_incr    in
      let result_incr = incr_map_fold map_incr   in
      let result_obs  = Incr.observe result_incr in
      let reset_counters () =
        f_count     := 0;
        f_inv_count := 0
      in
      let test_value () =
        (* Since [result_incr] was obtained as [incr_map_fold map_incr], check that the
           value of [result_incr] is equal to the result of applying the equivalent
           function [map_fold] directly to the value of [map_incr] *)
        let expected_result =
          map_fold (Incr.Observer.value_exn map_obs)
        in
        let actual_result = Incr.Observer.value_exn result_obs in
        [%test_result: [ `F of float | `F_inverse of float ] list Int.Map.t]
          actual_result ~expect:expected_result
      in
      let test_counters ~old_map ~new_map =
        (* It is expected that:
           - each time an entry is removed, [f_inverse] is called once
           - each time an entry is added, [f] is called once
           - each time an entry is replaced, [f] and [f_inverse] are each called once
        *)
        let expect =
          let symmetric_diff =
            Map.symmetric_diff old_map new_map ~data_equal:phys_equal
          in
          Sequence.fold symmetric_diff ~init:(0, 0)
            ~f:(fun (f_count, f_inv_count) (_, diff) ->
              match diff with
              | `Left _    -> f_count, f_inv_count + 1
              | `Right _   -> f_count + 1, f_inv_count
              | `Unequal _ -> f_count + 1, f_inv_count + 1)
        in
        [%test_result: int * int] ~expect (!f_count, !f_inv_count)
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
        then begin
          Incr.Var.set map_var map;
          stabilize_and_test_result ~old_map:!old_map ~new_map:map;
          old_map := map
        end;
        map)
      |> fun _map -> ()
    ;;

    let%test_unit "rand test: start with empty map, stabilize every step" =
      test_unordered_fold Int.Map.empty ~steps:100 ~stabilize_every_n:1
    ;;

    let%test_unit "rand test: start with non-empty map, stabilize every step" =
      let start_map = Rand_map_helper.init_rand_map ~from:0 ~to_:30 in
      test_unordered_fold start_map ~steps:100 ~stabilize_every_n:1
    ;;

    let%test_unit "rand test: start with empty map, stabilize every 10 steps" =
      test_unordered_fold Int.Map.empty ~steps:100 ~stabilize_every_n:10
    ;;
  end)


