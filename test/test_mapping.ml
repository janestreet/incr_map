open Core
open Poly
open Import

(** version of filter_mapi that tests the real implementation against a simple, all-at
    once one, and fails if the two implementations don't match.*)
let filter_mapi ~data_equal m ~f =
  let a = Incr.Map.filter_mapi ~data_equal m ~f in
  let b =
    let%map m = m in
    Map.filter_mapi ~f:(fun ~key ~data -> f ~key ~data) m
  in
  let%map a = a
  and b = b in
  require [%here] (Map.equal data_equal a b);
  a
;;

let%expect_test "simple filter_mapi" =
  let m = String.Map.of_alist_exn [ "foo", 3; "bar", 10; "snoo", 5 ] |> Incr.Var.create in
  let fm =
    filter_mapi ~data_equal:Int.equal (Incr.Var.watch m) ~f:(fun ~key:_ ~data:x ->
      let y = x * x in
      if y > 10 then Some y else None)
    |> Incr.observe
  in
  let dump () =
    Incr.stabilize ();
    [%sexp_of: int String.Map.t * string * int String.Map.t]
      (Incr.Var.value m, "->", Incr.Observer.value_exn fm)
    |> Sexp.to_string_hum
    |> print_endline
  in
  let change f =
    Incr.Var.set m (f (Incr.Var.value m));
    dump ()
  in
  dump ();
  [%expect {| (((bar 10) (foo 3) (snoo 5)) -> ((bar 100) (snoo 25))) |}];
  change (fun m -> Map.set m ~key:"foo" ~data:9);
  [%expect {| (((bar 10) (foo 9) (snoo 5)) -> ((bar 100) (foo 81) (snoo 25))) |}];
  change (fun m -> Map.set m ~key:"bar" ~data:1);
  [%expect {| (((bar 1) (foo 9) (snoo 5)) -> ((foo 81) (snoo 25))) |}];
  change (fun m -> Map.remove m "snoo");
  [%expect {| (((bar 1) (foo 9)) -> ((foo 81))) |}]
;;

module Map_operations = struct
  type 'a t =
    | Stabilize
    | Add of int * 'a
    | Remove of int
  [@@deriving sexp_of]

  let gen' ?keys_size ?operations data_gen =
    let open Quickcheck.Generator.Let_syntax in
    let%bind keys =
      let%bind len =
        match keys_size with
        | Some n -> return n
        | None -> Int.gen_incl 5 150
      in
      List.gen_with_length len Int.quickcheck_generator
      >>| List.dedup_and_sort ~compare:Int.compare
    in
    let key_gen = Quickcheck.Generator.of_list keys in
    let elt_gen =
      Quickcheck.Generator.weighted_union
        [ 1., return Stabilize
        ; ( 4.
          , let%map key = key_gen in
            Remove key )
        ; ( 10.
          , let%map key = key_gen
            and data = data_gen in
            Add (key, data) )
        ]
    in
    match operations with
    | None -> List.quickcheck_generator elt_gen
    | Some len -> List.gen_with_length len elt_gen
  ;;

  let quickcheck_generator data_gen = gen' data_gen

  let run_operations operations ~into:var ~after_stabilize =
    List.fold operations ~init:Int.Map.empty ~f:(fun map oper ->
      match oper with
      | Add (key, data) -> Map.set map ~key ~data
      | Remove key -> Map.remove map key
      | Stabilize ->
        Incr.Var.set var map;
        Incr.stabilize ();
        after_stabilize ();
        map)
    |> ignore
  ;;
end

let%test_unit "filter_mapi randomised fuzz test" =
  Quickcheck.test
    ~sexp_of:(List.sexp_of_t (Map_operations.sexp_of_t Int.sexp_of_t))
    (Map_operations.quickcheck_generator Int.quickcheck_generator)
    ~f:(fun operations ->
      let m = Incr.Var.create Int.Map.empty in
      let watch_m = Incr.Var.watch m
      and f ~key ~data =
        let y = data * data in
        Option.some_if (key + y > 33) y
      in
      let incr_filter_mapi =
        Incr.Map.filter_mapi watch_m ~data_equal:Int.equal ~f |> Incr.observe
      and slow_filter_mapi = Incr.map watch_m ~f:(Map.filter_mapi ~f) |> Incr.observe in
      Map_operations.run_operations operations ~into:m ~after_stabilize:(fun () ->
        [%test_result: int Int.Map.t]
          ~expect:(Incr.Observer.value_exn slow_filter_mapi)
          (Incr.Observer.value_exn incr_filter_mapi));
      Incr.Observer.disallow_future_use incr_filter_mapi;
      Incr.Observer.disallow_future_use slow_filter_mapi)
;;

let%bench_module "filter_mapi" =
  (module struct
    let test_data ~size ~operations =
      (* It is important this is deterministic *)
      Quickcheck.random_value
        ~seed:
          (`Deterministic
             (sprintf "%i-%i-%i-hello world, do not forget your towel" 42 size operations))
        (Map_operations.gen' Int.quickcheck_generator ~keys_size:size ~operations)
    ;;

    let benchmark_filter_mapi filter_mapi ~operations =
      let var = Incr.Var.create Int.Map.empty
      and test_map_fn ~key ~data =
        let y = data * data in
        Option.some_if (y + key > 33) y
      in
      let mapped =
        Incr.observe
          (filter_mapi ?data_equal:(Some Int.equal) (Incr.Var.watch var) ~f:test_map_fn)
      in
      Map_operations.run_operations operations ~into:var ~after_stabilize:(fun () ->
        ignore (Incr.Observer.value_exn mapped));
      Incr.Observer.disallow_future_use mapped
    ;;

    let%bench_fun ("random-ops"[@indexed operations = [ 5000; 10000; 100000 ]]) =
      let operations = test_data ~size:(operations / 100) ~operations in
      fun () -> benchmark_filter_mapi Incr.Map.filter_mapi ~operations
    ;;
  end)
;;

let%test_unit "filter_mapi' should not throw on empty map" =
  let observer =
    Incr.observe
      (Incr.Map.filter_mapi'
         ~f:(fun ~key:_ ~data -> Incr.map ~f:Option.return data)
         (Incr.Var.watch (Incr.Var.create Int.Map.empty)))
  in
  Incr.stabilize ();
  Incr.Observer.disallow_future_use observer
;;

type data =
  | F of bool
  | G of bool
  | H of bool
[@@deriving sexp, compare]

let%expect_test "check filter_mapi' against actual filter_mapi" =
  let input_var = Incr.Var.create Int.Map.empty in
  let f ~key x =
    match key % 3 with
    | 0 -> Some (F x)
    | 1 -> Some (G x)
    | 2 -> Option.some_if x (H x)
    | _ -> None
  in
  let watch_input = Incr.Var.watch input_var in
  let via_incr =
    Incr.Map.filter_mapi' watch_input ~f:(fun ~key ~data -> Incr.map data ~f:(f ~key))
    |> Incr.observe
  and via_map =
    Incr.map watch_input ~f:(fun map ->
      Map.filter_mapi map ~f:(fun ~key ~data -> f ~key data))
    |> Incr.observe
  in
  let test_with alist =
    Incr.Var.set input_var (Int.Map.of_alist_exn alist);
    Incr.stabilize ();
    print_s ([%sexp_of: data Int.Map.t] (Incr.Observer.value_exn via_incr));
    [%test_result: data Int.Map.t]
      ~expect:(Incr.Observer.value_exn via_map)
      (Incr.Observer.value_exn via_incr)
  in
  test_with [];
  [%expect {| () |}];
  test_with [ 1, true ];
  [%expect {| ((1 (G true))) |}];
  test_with [ 1, false ];
  [%expect {| ((1 (G false))) |}];
  test_with [ 1, false; 2, false ];
  [%expect {| ((1 (G false))) |}];
  test_with [ 1, false; 2, true ];
  [%expect {|
        ((1 (G false))
         (2 (H true))) |}];
  test_with [ 1, false; 2, false ];
  [%expect {| ((1 (G false))) |}];
  test_with [ 1, false; 2, true ];
  [%expect {|
        ((1 (G false))
         (2 (H true))) |}];
  test_with [ 1, true; 2, true; 3, false ];
  [%expect {|
        ((1 (G true))
         (2 (H true))
         (3 (F false))) |}];
  test_with [ 1, true; 2, true ];
  [%expect {|
        ((1 (G true))
         (2 (H true))) |}]
;;

let%test_module "random tests on filter and non-filter mapping functions" =
  (module struct
    module Filtering_or_not = struct
      type t =
        | Just_mapping
        | Filter_mapping
      [@@deriving enumerate]
    end

    type filtering_or_not = Filtering_or_not.t =
      | Just_mapping
      | Filter_mapping

    (* [f_count] is a counter used to keep track of how many times [f] is called by
       [Incr.Map.filter_mapi] / [Incr.Map.filter_mapi'] *)
    let f_count = ref 0

    (* [f] is the argument given to the [Just_mapping] functions *)
    let f key data = sprintf "%d: %f" key data

    (* [f] is the argument given to the [Filter_mapping] functions:
       [Map.filter_mapi] and [Incr.Map.filter_mapi], [Incr.Map.filter_mapi'] *)
    let f_opt key data = if key % 2 = 0 && data > 0.5 then None else Some (f key data)

    (* [incr_map_mapping_i], [incr_map_mapping_i'_with_map], and
       [incr_map_mapping_i'_with_bind] are different versions of the [Incr.Map] function
       being tested.  *)
    let incr_map_mapping_i map = function
      | Just_mapping ->
        Incr.Map.mapi map ~f:(fun ~key ~data ->
          incr f_count;
          f key data)
      | Filter_mapping ->
        Incr.Map.filter_mapi map ~f:(fun ~key ~data ->
          incr f_count;
          f_opt key data)
    ;;

    let incr_map_mapping_i'_with_map ?cutoff map = function
      | Just_mapping ->
        Incr.Map.mapi' ?cutoff map ~f:(fun ~key ~data:value_incr ->
          Incr.map value_incr ~f:(fun data ->
            incr f_count;
            f key data))
      | Filter_mapping ->
        Incr.Map.filter_mapi' ?cutoff map ~f:(fun ~key ~data:value_incr ->
          Incr.map value_incr ~f:(fun data ->
            incr f_count;
            f_opt key data))
    ;;

    let incr_map_mapping_i'_with_bind ?cutoff map = function
      | Just_mapping ->
        Incr.Map.mapi' ?cutoff map ~f:(fun ~key ~data:value_incr ->
          Incr.bind value_incr ~f:(fun data ->
            incr f_count;
            Incr.return (f key data)))
      | Filter_mapping ->
        Incr.Map.filter_mapi' ?cutoff map ~f:(fun ~key ~data:value_incr ->
          Incr.bind value_incr ~f:(fun data ->
            incr f_count;
            Incr.return (f_opt key data)))
    ;;

    (* [map_mapping_i] is the equivalent [Map] function that we test against *)
    let map_mapping_i map = function
      | Just_mapping -> Map.mapi map ~f:(fun ~key ~data -> f key data)
      | Filter_mapping -> Map.filter_mapi map ~f:(fun ~key ~data -> f_opt key data)
    ;;

    (* Stabilize and test the result as follows:
       - reset the counters to 0
       - call [Incr.stabilize ()]
       - check the value of [result_incr]
       - check the counter values
    *)
    let stabilize_and_test_result ~map_obs ~result_obs ~old_map ~new_map ~mapping =
      let reset_counter () = f_count := 0 in
      let test_value () =
        (* Since [result_incr] was obtained as [incr_map_mapping_i_fn map_incr], check
           that the value of [result_incr] is equal to the result of applying the
           equivalent function [map_mapping_i] directly to the value of [map_incr]
        *)
        [%test_result: string Int.Map.t]
          (Incr.Observer.value_exn result_obs)
          ~expect:(map_mapping_i (Incr.Observer.value_exn map_obs) mapping)
      in
      let test_counter ~old_map ~new_map =
        (* It is expected that [f] is called exactly once for each new or changed entry *)
        let expect =
          let symmetric_diff =
            Map.symmetric_diff old_map new_map ~data_equal:phys_equal
          in
          Sequence.fold symmetric_diff ~init:0 ~f:(fun acc (_, diff) ->
            match diff with
            | `Left _ -> acc
            | `Right _ | `Unequal _ -> acc + 1)
        in
        [%test_result: int] !f_count ~expect
      in
      reset_counter ();
      Incr.stabilize ();
      test_value ();
      test_counter ~old_map ~new_map
    ;;

    (* [Incr.Map.filter_mapi] and [Incr.Map.filter_mapi'] are tested as follows:

       First, create [map_incr] of type [float Int.Map.t Incr.t] with initial value [map].

       Next, apply the given [incr_map_mapping_i_fn] to [map_incr] to get [result_incr].

       At each of the [num_steps] steps, randomly change the value of [map_incr] by
       adding, removing, or replacing a single entry.

       Every [stabilize_every_n] steps, stabilize and check the result (see
       [stabilize_and_test_result] for details).
    *)
    let test_mapping_i map ~steps ~stabilize_every_n ~incr_map_mapping_i_fn =
      List.iter Filtering_or_not.all ~f:(fun mapping ->
        let map_var = Incr.Var.create map in
        let map_incr = Incr.Var.watch map_var in
        let map_obs = Incr.observe map_incr in
        let result_incr = incr_map_mapping_i_fn map_incr mapping in
        let result_obs = Incr.observe result_incr in
        stabilize_and_test_result
          ~map_obs
          ~result_obs
          ~old_map:Int.Map.empty
          ~new_map:map
          ~mapping;
        let old_map = ref map in
        List.fold (List.range 0 steps) ~init:map ~f:(fun map i ->
          let map = Rand_map_helper.rand_modify_map map in
          if i % stabilize_every_n = 0
          then (
            Incr.Var.set map_var map;
            stabilize_and_test_result
              ~map_obs
              ~result_obs
              ~old_map:!old_map
              ~new_map:map
              ~mapping;
            old_map := map);
          map)
        |> fun _map -> ())
    ;;

    let%test_unit "mapping_i rand test: start with empty map, stabilize every step" =
      test_mapping_i
        Int.Map.empty
        ~steps:100
        ~stabilize_every_n:1
        ~incr_map_mapping_i_fn:incr_map_mapping_i
    ;;

    let%test_unit "mapping_i rand test: start with non-empty map, stabilize every step" =
      let start_map = Rand_map_helper.init_rand_map ~from:0 ~to_:30 in
      test_mapping_i
        start_map
        ~steps:100
        ~stabilize_every_n:1
        ~incr_map_mapping_i_fn:incr_map_mapping_i
    ;;

    let%test_unit "mapping_i rand test: start with empty map, stabilize every 10 steps" =
      test_mapping_i
        Int.Map.empty
        ~steps:100
        ~stabilize_every_n:10
        ~incr_map_mapping_i_fn:incr_map_mapping_i
    ;;

    let%test_unit "mapping_i' with map rand test: start with empty map, stabilize every \
                   step"
      =
      test_mapping_i
        Int.Map.empty
        ~steps:100
        ~stabilize_every_n:1
        ~incr_map_mapping_i_fn:incr_map_mapping_i'_with_map
    ;;

    let%test_unit "mapping_i' with map rand test: start with non-empty map, stabilize \
                   every step"
      =
      let start_map = Rand_map_helper.init_rand_map ~from:0 ~to_:30 in
      test_mapping_i
        start_map
        ~steps:100
        ~stabilize_every_n:1
        ~incr_map_mapping_i_fn:incr_map_mapping_i'_with_map
    ;;

    let%test_unit "mapping_i' with map rand test: start with empty map, stabilize every \
                   10 steps"
      =
      test_mapping_i
        Int.Map.empty
        ~steps:100
        ~stabilize_every_n:10
        ~incr_map_mapping_i_fn:incr_map_mapping_i'_with_map
    ;;

    let%test_unit "mapping_i' with bind rand test: start with empty map, stabilize every \
                   step"
      =
      test_mapping_i
        Int.Map.empty
        ~steps:100
        ~stabilize_every_n:1
        ~incr_map_mapping_i_fn:incr_map_mapping_i'_with_bind
    ;;

    let%test_unit "mapping_i' with bind rand test: start with empty map, stabilize every \
                   10 steps"
      =
      test_mapping_i
        Int.Map.empty
        ~steps:100
        ~stabilize_every_n:10
        ~incr_map_mapping_i_fn:incr_map_mapping_i'_with_bind
    ;;

    (* [incr_map_mapping_i'_with_map_and_cutoff] and
       [incr_map_mapping_i'_with_bind_and_cutoff] are two more versions of the
       [Incr.Map] function being tested, but they are passed an additional [cutoff]
       argument equal to [Incr.Cutoff.always].
    *)
    let incr_map_mapping_i'_with_map_and_cutoff map =
      incr_map_mapping_i'_with_map ~cutoff:Incr.Cutoff.always map
    ;;

    let incr_map_mapping_i'_with_bind_and_cutoff map =
      incr_map_mapping_i'_with_bind ~cutoff:Incr.Cutoff.always map
    ;;

    (* [stabilize_and_test_result_with_cutoff] is like [stabilize_and_test_result] but
       expects different values for the counter and [result_incr] due to the cutoff.
    *)
    let stabilize_and_test_result_with_cutoff ~initial_result ~result_obs =
      let reset_counter () = f_count := 0 in
      let test_value () =
        (* Check that the value of [result_incr] is equal to the initial result value,
           since any changes in [map_incr] should have been cut off and not propagated to
           [result_incr]
        *)
        [%test_result: string Int.Map.t]
          ~expect:initial_result
          (Incr.Observer.value_exn result_obs)
      in
      let test_counter () =
        (* It is expected that [f] is not called at all due to the cutoff. *)
        [%test_result: int] !f_count ~expect:0
      in
      reset_counter ();
      Incr.stabilize ();
      test_value ();
      test_counter ()
    ;;

    (* [test_mapping_i_with_cutoff] is similar to [test_filter_mapi] but tests a
       [Incr.Map.filter_mapi'] function with a [cutoff] argument equal to
       [Incr.Cutoff.always]
    *)
    let test_mapping_i_with_cutoff map ~steps ~incr_map_mapping_i_fn_with_cutoff =
      List.iter Filtering_or_not.all ~f:(fun mapping ->
        let map_var = Incr.Var.create map in
        let map_incr = Incr.Var.watch map_var in
        let map_obs = Incr.observe map_incr in
        let result_incr = incr_map_mapping_i_fn_with_cutoff map_incr mapping in
        let result_obs = Incr.observe result_incr in
        stabilize_and_test_result
          ~map_obs
          ~result_obs
          ~old_map:Int.Map.empty
          ~new_map:map
          ~mapping;
        (* When a new entry is added, the [cutoff] has no impact on the result so
           we use the same [stabilize_and_test_result] as before. *)
        let old_map = ref map in
        List.fold
          (List.range 0 (steps / 2))
          ~init:map
          ~f:(fun map _i ->
            let map = Rand_map_helper.rand_add_to_map map in
            Incr.Var.set map_var map;
            stabilize_and_test_result
              ~map_obs
              ~result_obs
              ~old_map:!old_map
              ~new_map:map
              ~mapping;
            old_map := map;
            map)
        |> fun map ->
        (* When an entry is removed, the [cutoff] has no impact on the result so
           we use the same [stabilize_and_test_result] as before. *)
        let old_map = ref map in
        List.fold
          (List.range 0 (steps / 4))
          ~init:map
          ~f:(fun map _i ->
            let map = Rand_map_helper.rand_remove_from_map map in
            Incr.Var.set map_var map;
            stabilize_and_test_result
              ~map_obs
              ~result_obs
              ~old_map:!old_map
              ~new_map:map
              ~mapping;
            old_map := map;
            map)
        |> fun map ->
        let initial_result = map_mapping_i map mapping in
        (* When the data for an existing key is changed in [map_var], the [cutoff] kicks in
           to prevent the new data from propagating forward, so we expect [f] to not be
           called and the [result_incr] to not be updated.
           We check this using [stabilize_and_test_result_with_cutoff]. *)
        List.fold
          (List.range 0 (steps / 4))
          ~init:map
          ~f:(fun map _i ->
            let map = Rand_map_helper.rand_replace_in_map map in
            Incr.Var.set map_var map;
            stabilize_and_test_result_with_cutoff ~initial_result ~result_obs;
            map)
        |> fun _map -> ())
    ;;

    let%test_unit "mapping_i' with map and cutoff rand test" =
      let start_map = Rand_map_helper.init_rand_map ~from:0 ~to_:30 in
      test_mapping_i_with_cutoff
        start_map
        ~steps:100
        ~incr_map_mapping_i_fn_with_cutoff:incr_map_mapping_i'_with_map_and_cutoff
    ;;

    let%test_unit "mapping_i' with bind and cutoff rand test" =
      let start_map = Rand_map_helper.init_rand_map ~from:0 ~to_:30 in
      test_mapping_i_with_cutoff
        start_map
        ~steps:100
        ~incr_map_mapping_i_fn_with_cutoff:incr_map_mapping_i'_with_bind_and_cutoff
    ;;
  end)
;;
