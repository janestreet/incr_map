open Core
open Import

let%test_module "random tests" =
  (module struct
    (* [Incr.Map.flatten] is tested as follows:

       First, create [map_of_incrs] of type [float Incr.t Int.Map.t] with initial values
       equal to those in [map].

       Next, apply [Incr.Map.flatten] to [map_of_incrs] to get [result_incr].

       At each of the [num_steps] steps, randomly change a single entry in [map_of_incrs]
       by setting the [Incr.Var.t] corresponding to its data to a new value.

       Every [stabilize_every_n] steps, check the result as follows:
       - call [Incr.stabilize ()]
       - check the value of [result_incr]
    *)
    let test_flatten map ~num_steps ~stabilize_every_n =
      let map_of_vars = Map.map map ~f:Incr.Var.create in
      let map_of_incrs = Map.map map_of_vars ~f:Incr.Var.watch in
      let result_incr = Incr.Map.flatten map_of_incrs in
      let result_obs = Incr.observe result_incr in
      let test_value () =
        (* Since [result_incr] was obtained as [Incr.Map.flatten map_of_incrs], check the
           value of [result_incr] against the data values in [map_of_incrs] *)
        [%test_result: float Int.Map.t]
          (Incr.Observer.value_exn result_obs)
          ~expect:(Map.map map_of_vars ~f:Incr.Var.value)
      in
      let stabilize_and_test_result () =
        Incr.stabilize ();
        test_value ()
      in
      stabilize_and_test_result ();
      List.iter (List.range 0 num_steps) ~f:(fun i ->
        Rand_map_helper.rand_set_in_map_of_vars map_of_vars;
        if i % stabilize_every_n = 0 then stabilize_and_test_result ())
    ;;

    let%test_unit "rand test: stabilize every step" =
      let start_map = Rand_map_helper.init_rand_map ~from:0 ~to_:30 in
      test_flatten start_map ~num_steps:100 ~stabilize_every_n:1
    ;;

    let%test_unit "rand test: stabilize every 10 steps" =
      let start_map = Rand_map_helper.init_rand_map ~from:0 ~to_:30 in
      test_flatten start_map ~num_steps:100 ~stabilize_every_n:10
    ;;

    (* [test_flatten_with_cutoff] is similar to [test_flatten].
       However, here the cutoffs of all the data in [map_of_incrs] are set to [always],
       which means changes in [map_of_incrs] should not porpagate to [result_incr]
    *)
    let test_flatten_with_cutoff map ~num_steps =
      let map_of_vars = Map.map map ~f:Incr.Var.create in
      let map_of_incrs = Map.map map_of_vars ~f:Incr.Var.watch in
      let result_incr = Incr.Map.flatten map_of_incrs in
      let result_obs = Incr.observe result_incr in
      (* set the cutoff of the data in [map_of_incrs] to [always] *)
      Map.iter map_of_incrs ~f:(fun incr -> Incr.set_cutoff incr Incr.Cutoff.always);
      let test_value () =
        (* Check the value of [result_incr] against the initial data values of
           [map_of_incrs], which are equal to the values in [map] *)
        Incr.stabilize ();
        [%test_result: float Int.Map.t] (Incr.Observer.value_exn result_obs) ~expect:map
      in
      let stabilize_and_test_result () =
        Incr.stabilize ();
        test_value ()
      in
      stabilize_and_test_result ();
      for _ = 0 to num_steps do
        Rand_map_helper.rand_set_in_map_of_vars map_of_vars;
        stabilize_and_test_result ()
      done
    ;;

    let%test_unit "rand test: stabilize every step, always cut off" =
      let start_map = Rand_map_helper.init_rand_map ~from:0 ~to_:30 in
      test_flatten_with_cutoff start_map ~num_steps:10
    ;;
  end)
;;
