open Core
open Poly
open Import

let%test_module "random tests" =
  (module struct
    (* [f_left_count], [f_right_count] and [f_both_count] are counters used to keep track
       of how many times the [`Left], [`Right], and [`Both] branches of [f] respectively
       are run as a result of [f] being called by [Incr.Map.merge]. *)
    let f_left_count = ref 0
    let f_right_count = ref 0
    let f_both_count = ref 0

    (* [f] is the argument given to [Incr.Map.merge] and [Map.merge].
       The counters are only incremented when [f] is called by [Incr.Map.merge]. *)
    let f ~key:_ data ~incr_counters =
      match data with
      | `Left v ->
        if incr_counters then incr f_left_count;
        Some v
      | `Right v ->
        if incr_counters then incr f_right_count;
        Some v
      | `Both (v1, v2) ->
        if incr_counters then incr f_both_count;
        Some (v1 +. v2)
    ;;

    (* [incr_map_merge] is the first [Incr.Map] function being tested *)
    let incr_map_merge map1 map2 = Incr.Map.merge map1 map2 ~f:(f ~incr_counters:true)

    (* [incr_map_merge'] is the second [Incr.Map] function being tested *)
    let incr_map_merge' map1 map2 =
      Incr.Map.merge' map1 map2 ~f:(fun ~key diff ->
        let%map diff = diff in
        f ~incr_counters:true ~key diff)
    ;;

    (* [map_merge] is the equivalent [Map] function that we test against *)
    let map_merge map1 map2 = Map.merge map1 map2 ~f:(f ~incr_counters:false)

    module Change_status = struct
      type present =
        [ `Still_present_unchanged
        | `Value_set
        ]

      type missing =
        [ `Still_absent
        | `Removed
        ]

      type unchanged =
        [ `Still_present_unchanged
        | `Still_absent
        ]
    end

    (* [Incr.Map.merge] and [Incr.Map.merge'] are tested as follows:

       First, create [map_incr1] and [map_incr2] of type [float Int.Map.t Incr.t] with
       initial values [map1] and [map2] respectively.

       Next, apply [incr_map_merge] or [incr_map_merge'] (chosen based on the value of
       [use_merge']) to [map_incr1] and [map_incr2] to get [result_incr].

       At each of the [num_steps] steps, randomly change the value of exactly one of
       [map_incr1] and [map_incr2] by adding, removing, or replacing a single entry.

       Every [stabilize_every_n] steps, check the result as follows:
       - reset the counters to 0
       - call [Incr.stabilize ()]
       - check the value of [result_incr]
       - check the counter values
    *)
    let test_merge map1 map2 ~steps ~stabilize_every_n ~use_merge' =
      let map1_var, map2_var = Incr.Var.create map1, Incr.Var.create map2 in
      let map1_incr, map2_incr = Incr.Var.watch map1_var, Incr.Var.watch map2_var in
      let map1_obs, map2_obs = Incr.observe map1_incr, Incr.observe map2_incr in
      let incr_map_merge_fn = if use_merge' then incr_map_merge' else incr_map_merge in
      let result_incr = incr_map_merge_fn map1_incr map2_incr in
      let result_obs = Incr.observe result_incr in
      let reset_counters () =
        f_left_count := 0;
        f_right_count := 0;
        f_both_count := 0
      in
      let test_value () =
        (* Since [result_incr] was obtained by applying [incr_map_merge] or
           [incr_map_merge'] to [map_incr1] and [map_incr2], check that the value of
           [result_incr] is equal to the result of applying the equivalent function
           [map_merge] directly to the values of [map_incr1] and [map_incr2] *)
        Incr.stabilize ();
        let expect =
          map_merge (Incr.Observer.value_exn map1_obs) (Incr.Observer.value_exn map2_obs)
        in
        [%test_result: float Int.Map.t] (Incr.Observer.value_exn result_obs) ~expect
      in
      let test_counters ~old_map1 ~new_map1 ~old_map2 ~new_map2 =
        (* Check that the provided [~f] function is called exactly when we think it is
           (and with the right arguments). We do this by tracking how every key changes
           in both maps, inferring the number of [~f] calls of every type from that, and
           comparing it to the actual numbers of calls made.*)
        let expect =
          let symdiff_element_to_key_change_state = function
            | `Left _ -> `Removed
            | `Unequal _ | `Right _ -> `Value_set
          in
          let create_symdiff_map old_map new_map =
            Map.symmetric_diff old_map new_map ~data_equal:phys_equal
            |> Sequence.to_list
            |> List.map ~f:(fun (key, value) ->
              key, symdiff_element_to_key_change_state value)
            |> Int.Map.of_alist_exn
          in
          let symdiff_map1, symdiff_map2 =
            create_symdiff_map old_map1 new_map1, create_symdiff_map old_map2 new_map2
          in
          let keys_touched =
            List.rev_append (Map.keys symdiff_map1) (Map.keys symdiff_map2)
            |> List.dedup_and_sort ~compare:Int.compare
          in
          let extend_symdiff_map symdiff_map keys_touched new_map =
            List.fold keys_touched ~init:symdiff_map ~f:(fun symdiff_map key ->
              if Map.mem symdiff_map key
              then symdiff_map
              else
                Map.set
                  symdiff_map
                  ~key
                  ~data:
                    (if Map.mem new_map key
                     then `Still_present_unchanged
                     else `Still_absent))
          in
          let symdiff_map1 = extend_symdiff_map symdiff_map1 keys_touched new_map1 in
          let symdiff_map2 = extend_symdiff_map symdiff_map2 keys_touched new_map2 in
          let merged =
            Map.merge symdiff_map1 symdiff_map2 ~f:(fun ~key:_ data ->
              match data with
              | `Both (v1, v2) -> Some (v1, v2)
              | _ -> failwith "Test is broken, key sets differ")
          in
          Map.fold merged ~init:(0, 0, 0) ~f:(fun ~key:_ ~data (left, right, both) ->
            match data with
            | #Change_status.unchanged, #Change_status.unchanged ->
              failwith "Test is broken, diff of key with no change"
            | #Change_status.present, #Change_status.present -> left, right, both + 1
            | #Change_status.present, #Change_status.missing -> left + 1, right, both
            | #Change_status.missing, #Change_status.present -> left, right + 1, both
            | #Change_status.missing, #Change_status.missing -> left, right, both)
        in
        [%test_result: int * int * int]
          ~expect
          (!f_left_count, !f_right_count, !f_both_count)
      in
      let stabilize_and_test_result ~old_map1 ~new_map1 ~old_map2 ~new_map2 =
        reset_counters ();
        Incr.stabilize ();
        test_value ();
        test_counters ~old_map1 ~new_map1 ~old_map2 ~new_map2
      in
      stabilize_and_test_result
        ~old_map1:Int.Map.empty
        ~new_map1:map1
        ~old_map2:Int.Map.empty
        ~new_map2:map2;
      let old_map1, old_map2 = ref map1, ref map2 in
      List.fold (List.range 0 steps) ~init:(map1, map2) ~f:(fun (map1, map2) i ->
        let map1, map2 =
          if Rand_map_helper.rand () < 0.5
          then Rand_map_helper.rand_modify_map map1, map2
          else map1, Rand_map_helper.rand_modify_map map2
        in
        if i % stabilize_every_n = 0
        then (
          Incr.Var.set map1_var map1;
          Incr.Var.set map2_var map2;
          stabilize_and_test_result
            ~old_map1:!old_map1
            ~new_map1:map1
            ~old_map2:!old_map2
            ~new_map2:map2;
          old_map1 := map1;
          old_map2 := map2);
        map1, map2)
      |> fun (_map1, _map2) -> ()
    ;;

    let%test_unit "rand test: start with two empty maps, stabilize every step" =
      List.iter Bool.all ~f:(fun use_merge' ->
        test_merge
          Int.Map.empty
          Int.Map.empty
          ~steps:500
          ~stabilize_every_n:1
          ~use_merge')
    ;;

    let%test_unit "rand test: start with empty and non-empty map, stabilize every step" =
      List.iter Bool.all ~f:(fun use_merge' ->
        let start_map2 = Rand_map_helper.init_rand_map ~from:0 ~to_:30 in
        test_merge Int.Map.empty start_map2 ~steps:500 ~stabilize_every_n:1 ~use_merge')
    ;;

    let%test_unit "rand test: start with non-empty and empty map, stabilize every step" =
      List.iter Bool.all ~f:(fun use_merge' ->
        let start_map1 = Rand_map_helper.init_rand_map ~from:0 ~to_:30 in
        test_merge start_map1 Int.Map.empty ~steps:500 ~stabilize_every_n:1 ~use_merge')
    ;;

    let%test_unit "rand test: start with two non-empty maps, stabilize every step" =
      List.iter Bool.all ~f:(fun use_merge' ->
        let start_map1 = Rand_map_helper.init_rand_map ~from:0 ~to_:30 in
        let start_map2 = Rand_map_helper.init_rand_map ~from:20 ~to_:40 in
        test_merge start_map1 start_map2 ~steps:1000 ~stabilize_every_n:1 ~use_merge')
    ;;

    let%test_unit "rand test: stat with two empty maps, stabilize every 10 steps" =
      List.iter Bool.all ~f:(fun use_merge' ->
        test_merge
          Int.Map.empty
          Int.Map.empty
          ~steps:500
          ~stabilize_every_n:10
          ~use_merge')
    ;;
  end)
;;

let%bench_module "merge" =
  (module struct
    type map = int Int.Map.t

    let gen_map (min_key, max_key) ~size =
      if size > max_key - min_key + 1
      then
        raise_s
          [%message
            "Cannot generate a map with more keys than the available range."
              (min_key : int)
              (max_key : int)
              (size : int)];
      let open Quickcheck.Generator.Let_syntax in
      let%map keys =
        let all_keys_length = max_key - min_key + 1 in
        let%map all_keys =
          List.gen_permutations
            (List.range ~start:`inclusive min_key ~stop:`inclusive max_key)
        in
        List.drop all_keys (all_keys_length - size)
      and values = List.gen_with_length size Int.quickcheck_generator in
      Int.Map.of_alist_exn (List.zip_exn keys values)
    ;;

    type t =
      { left_input : map Incr.Var.t
      ; right_input : map Incr.Var.t
      ; output : map Incr.Observer.t
      }

    let create ~merge_f left_input right_input =
      let left_input = Incr.Var.create left_input in
      let right_input = Incr.Var.create right_input in
      let output =
        Incr.Map.merge (Incr.Var.watch left_input) (Incr.Var.watch right_input) ~f:merge_f
        |> Incr.observe
      in
      { left_input; right_input; output }
    ;;

    module Map_modification = struct
      type t =
        | Add of int * int
        | Remove of int

      let quickcheck_generator key_range =
        let open Quickcheck.Generator.Let_syntax in
        let key_gen (key_range_begin, key_range_end) =
          Int.gen_incl key_range_begin key_range_end
        in
        let add_gen =
          let%map key = key_gen key_range
          and data = Int.quickcheck_generator in
          Add (key, data)
        and remove_gen =
          let%map key = key_gen key_range in
          Remove key
        in
        Quickcheck.Generator.union [ add_gen; remove_gen ]
      ;;

      let perform t map =
        match t with
        | Add (key, data) -> Map.set map ~key ~data
        | Remove key -> Map.remove map key
      ;;
    end

    module Operation = struct
      type t =
        | Stabilize
        | Modify_left of Map_modification.t
        | Modify_right of Map_modification.t

      let quickcheck_generator key_range =
        let open Quickcheck.Generator.Let_syntax in
        let modify_gen =
          let%map dir =
            Quickcheck.Generator.of_list
              [ (fun x -> Modify_left x); (fun x -> Modify_right x) ]
          and modification = Map_modification.quickcheck_generator key_range in
          dir modification
        in
        Quickcheck.Generator.weighted_union [ 1., return Stabilize; 10., modify_gen ]
      ;;

      let perform op t =
        match op with
        | Modify_left md ->
          Incr.Var.set
            t.left_input
            (Incr.Var.latest_value t.left_input |> Map_modification.perform md)
        | Modify_right md ->
          Incr.Var.set
            t.right_input
            (Incr.Var.latest_value t.right_input |> Map_modification.perform md)
        | Stabilize -> Incr.stabilize ()
      ;;
    end

    let generate_fun ~length ~key_range ~initial_size ~merge_f =
      let open Quickcheck.Generator.Let_syntax in
      let gen_fun =
        let gen_map = gen_map key_range ~size:initial_size in
        let%map operations =
          List.gen_with_length length (Operation.quickcheck_generator key_range)
        and left_input = gen_map
        and right_input = gen_map in
        let t = create ~merge_f left_input right_input in
        Staged.stage (fun () ->
          List.iter operations ~f:(fun op -> Operation.perform op t))
      in
      Quickcheck.random_value
        gen_fun
        ~seed:(`Deterministic (sprintf "%i-%i-bla-bla-bla-foo-bar-baz" 43 length))
    ;;

    let key_range = 0, 25000
    let initial_size = 15000

    let%bench_fun ("cheap merging function"[@indexed length = [ 1000; 3000; 5000 ]]) =
      generate_fun ~length ~key_range ~initial_size ~merge_f:(fun ~key:_ v ->
        match v with
        | `Left v | `Right v | `Both (v, _) -> Some v)
      |> Staged.unstage
    ;;

    let%bench_fun ("merging function with expensive `Both case"[@indexed
                     length
                     = [ 1000; 2000; 4000 ]])
      =
      generate_fun ~length ~key_range ~initial_size ~merge_f:(fun ~key:_ v ->
        match v with
        | `Left v | `Right v -> Some v
        | `Both (v, _) ->
          Time.pause (Time.Span.of_us 1.);
          Some v)
      |> Staged.unstage
    ;;
  end)
;;
