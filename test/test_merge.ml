open Core.Std
open Import

let%test_module "random tests" =
  (module struct

    (* [f_left_count], [f_right_count] and [f_both_count] are counters used to keep track
       of how many times the [`Left], [`Right], and [`Both] branches of [f] respectively
       are run as a result of [f] being called by [Incr.Map.merge]. *)
    let f_left_count  = ref 0
    let f_right_count = ref 0
    let f_both_count  = ref 0

    (* [f] is the argument given to [Incr.Map.merge] and [Map.merge].
       The counters are only incremented when [f] is called by [Incr.Map.merge]. *)
    let f ~key:_ data ~incr_counters =
      match data with
      | `Left  v       -> if incr_counters then incr f_left_count ; Some v
      | `Right v       -> if incr_counters then incr f_right_count; Some v
      | `Both (v1, v2) -> if incr_counters then incr f_both_count ; Some (v1 +. v2)
    ;;

    (* [incr_map_merge] is the [Incr.Map] function being tested *)
    let incr_map_merge map1 map2 = Incr.Map.merge map1 map2 ~f:(f ~incr_counters:true)

    (* [map_merge] is the equivalent [Map] function that we test against *)
    let map_merge map1 map2 = Map.merge map1 map2 ~f:(f ~incr_counters:false)

    (* [Incr.Map.merge] is tested as follows:

       First, create [map_incr1] and [map_incr2] of type [float Int.Map.t Incr.t] with
       initial values [map1] and [map2] respectively.

       Next, apply [incr_map_merge] to [map_incr1] and [map_incr2] to get [result_incr].

       At each of the [num_steps] steps, randomly change the value of exactly one of
       [map_incr1] and [map_incr2] by adding, removing, or replacing a single entry.

       Every [stabilize_every_n] steps, check the result as follows:
       - reset the counters to 0
       - call [Incr.stabilize ()]
       - check the value of [result_incr]
       - check the counter values
    *)
    let test_merge map1 map2 ~steps ~stabilize_every_n =
      let map1_var , map2_var  = Incr.Var.create map1   , Incr.Var.create map2    in
      let map1_incr, map2_incr = Incr.Var.watch map1_var, Incr.Var.watch map2_var in
      let map1_obs , map2_obs  = Incr.observe map1_incr , Incr.observe map2_incr  in
      let result_incr = incr_map_merge map1_incr map2_incr in
      let result_obs  = Incr.observe result_incr           in
      let reset_counters () =
        f_left_count  := 0;
        f_right_count := 0;
        f_both_count  := 0
      in
      let test_value () =
        (* Since [result_incr] was obtained as [incr_map_mereg map_incr1 map_incr2],
           check that the value of [result_incr] is equal to the result of applying the
           equivalent function [map_merge] directly to the values of [map_incr1] and
           [map_incr2] *)
        Incr.stabilize ();
        let expect =
          map_merge
            (Incr.Observer.value_exn map1_obs)
            (Incr.Observer.value_exn map2_obs)
        in
        [%test_result: float Int.Map.t] (Incr.Observer.value_exn result_obs) ~expect
      in
      let test_counters ~old_map1 ~new_map1 ~old_map2 ~new_map2 =
        (* It is expected that for a given map (either map1 or map2):
           - each time a key is removed from the map:
              - if it exists in the other map, the other map's count is incremented by 1
              - if it doesn't exist in the other map, nothing is incremented
           - each time a key is added to the map or the data for the key is modified:
              - if it exists in the other map, the "both" count is incremented by 1
              - if it doesn't exist in the other map, this map's count is incremented by 1
        *)
        let expect =
          let fold_f (this_count, other_count, both_count) (key, diff) ~other_new_map =
            match diff with
            | `Left _    ->
              if Map.mem other_new_map key
              then this_count, other_count + 1, both_count
              else this_count, other_count    , both_count
            | `Right _ | `Unequal _ ->
              if Map.mem other_new_map key
              then this_count    , other_count, both_count + 1
              else this_count + 1, other_count, both_count
          in
          Sequence.fold (Map.symmetric_diff old_map1 new_map1 ~data_equal:phys_equal)
            ~init:(0, 0, 0)
            ~f:(fold_f ~other_new_map:new_map2)
          |> fun (f_left_count, f_right_count, f_both_count) ->
          Sequence.fold (Map.symmetric_diff old_map2 new_map2 ~data_equal:phys_equal)
            ~init:(f_right_count, f_left_count, f_both_count)
            ~f:(fold_f ~other_new_map:new_map1)
          |> fun (f_right_count, f_left_count, f_both_count) ->
          f_left_count, f_right_count, f_both_count
        in
        [%test_result: int * int * int] ~expect
          (!f_left_count, !f_right_count, !f_both_count)
      in
      let stabilize_and_test_result ~old_map1 ~new_map1 ~old_map2 ~new_map2 =
        reset_counters ();
        Incr.stabilize ();
        test_value ();
        test_counters ~old_map1 ~new_map1 ~old_map2 ~new_map2
      in
      stabilize_and_test_result ~old_map1:Int.Map.empty ~new_map1:map1
        ~old_map2:Int.Map.empty ~new_map2:map2;
      let old_map1, old_map2 = ref map1, ref map2 in
      List.fold (List.range 0 steps) ~init:(map1, map2) ~f:(fun (map1, map2) i ->
        let map1, map2 =
          if Rand_map_helper.rand () < 0.5
          then Rand_map_helper.rand_modify_map map1, map2
          else map1, Rand_map_helper.rand_modify_map map2
        in
        if i % stabilize_every_n = 0
        then begin
          Incr.Var.set map1_var map1;
          Incr.Var.set map2_var map2;
          stabilize_and_test_result ~old_map1:!old_map1 ~new_map1:map1
            ~old_map2:!old_map2 ~new_map2:map2;
          old_map1 := map1;
          old_map2 := map2
        end;
        map1, map2)
      |> fun (_map1, _map2) -> ()
    ;;

    let%test_unit "rand test: start with two empty maps, stabilize every step" =
      test_merge Int.Map.empty Int.Map.empty ~steps:500 ~stabilize_every_n:1
    ;;

    let%test_unit "rand test: start with empty and non-empty map, stabilize every step" =
      let start_map2 = Rand_map_helper.init_rand_map ~from:0 ~to_:30 in
      test_merge Int.Map.empty start_map2 ~steps:500
        ~stabilize_every_n:1
    ;;

    let%test_unit "rand test: start with non-empty and empty map, stabilize every step" =
      let start_map1 = Rand_map_helper.init_rand_map ~from:0 ~to_:30 in
      test_merge start_map1 Int.Map.empty ~steps:500 ~stabilize_every_n:1
    ;;

    let%test_unit "rand test: start with two non-empty maps, stabilize every step" =
      let start_map1 = Rand_map_helper.init_rand_map ~from:0  ~to_:30 in
      let start_map2 = Rand_map_helper.init_rand_map ~from:20 ~to_:40 in
      test_merge start_map1 start_map2 ~steps:1000 ~stabilize_every_n:1
    ;;

    let%test_unit "rand test: stat with two empty maps, stabilize every 10 steps" =
      test_merge Int.Map.empty Int.Map.empty ~steps:500 ~stabilize_every_n:10
    ;;
  end)
