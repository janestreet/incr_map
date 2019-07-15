open Core
open Import

let%test_unit "correctness" =
  let with_init_value init f =
    let map_var = Incr.Var.create init in
    let set_obs = map_var |> Incr.Var.watch |> Incr.Map.keys |> Incr.observe in
    let update_and_check m =
      let expect = Map.key_set m in
      Incr.Var.set map_var m;
      Incr.stabilize ();
      [%test_result: Int.Set.t] ~expect (Incr.Observer.value_exn set_obs)
    in
    f update_and_check
  in
  List.iter [ 0; 16; 32 ] ~f:(fun len ->
    let init = Int.Map.of_alist_exn (List.init len ~f:(fun i -> i, i)) in
    with_init_value init (fun update_and_check ->
      let current_set = ref init in
      Quickcheck.iter
        ~trials:100
        [%quickcheck.generator:
          bool
          * [%custom Quickcheck.Generator.small_positive_int]
          * [%custom Quickcheck.Generator.small_positive_int]]
        ~f:(fun (add, key, data) ->
          let updated_map =
            match add with
            | true -> Map.set !current_set ~key ~data
            | false -> Map.remove !current_set key
          in
          current_set := updated_map;
          update_and_check updated_map));
    with_init_value init (fun update_and_check ->
      Quickcheck.test
        ~examples:[ [] ]
        ~trials:100
        [%quickcheck.generator:
          [%custom Quickcheck.Generator.small_positive_int] list]
        ~f:(fun l ->
          l |> List.mapi ~f:Tuple2.create |> Int.Map.of_alist_exn |> update_and_check)))
;;
