open Core
open Import

let%test_unit "correctness" =
  let with_init_value init f =
    let set_var = Incr.Var.create init in
    let map_obs = set_var |> Incr.Var.watch |> Incr.Map.of_set |> Incr.observe in
    let update_and_check s =
      let comparator = Set.comparator s in
      let expect =
        s
        |> Set.to_list
        |> List.map ~f:(fun i -> i, ())
        |> Map.Using_comparator.of_alist_exn ~comparator
      in
      Incr.Var.set set_var s;
      Incr.stabilize ();
      [%test_result: unit Int.Map.t] ~expect (Incr.Observer.value_exn map_obs)
    in
    f update_and_check
  in
  List.iter [ 0; 16; 32 ] ~f:(fun len ->
    let init = Int.Set.of_list (List.init len ~f:Fn.id) in
    with_init_value init (fun update_and_check ->
      let current_set = ref init in
      Quickcheck.iter
        ~trials:100
        (Quickcheck.Generator.tuple2
           Quickcheck.Generator.bool
           Quickcheck.Generator.small_positive_int)
        ~f:(fun (add, key) ->
          let change = if add then Set.add else Set.remove in
          current_set := change !current_set key;
          update_and_check !current_set));
    with_init_value init (fun update_and_check ->
      Quickcheck.test
        ~examples:[ [] ]
        ~trials:100
        (Quickcheck.Generator.list Quickcheck.Generator.small_positive_int)
        ~f:(fun l -> update_and_check (Int.Set.of_list l))))
;;
