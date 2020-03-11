open! Core
open! Import

let%test_module _ =
  (module struct
    let sum_nested_maps_incr map ~use_update =
      Incr.Map.unordered_fold_nested_maps
        ?update:
          (Option.some_if
             use_update
             (fun ~outer_key:_ ~inner_key:_ ~old_data ~new_data acc ->
                acc - old_data + new_data))
        ~data_equal:( = )
        map
        ~init:0
        ~add:(fun ~outer_key:_ ~inner_key:_ ~data:v acc -> acc + v)
        ~remove:(fun ~outer_key:_ ~inner_key:_ ~data:v acc -> acc - v)
    ;;

    let sum_nested_maps_all_at_once map =
      let%map map = map in
      Map.fold
        ~init:0
        ~f:(fun ~key:_ ~data:inner_map acc ->
          Map.fold inner_map ~init:acc ~f:(fun ~key:_ ~data acc -> data + acc))
        map
    ;;

    let update_and_test ~initial_map ~dump =
      let map_var = Incr.Var.create initial_map in
      let map = Incr.Var.watch map_var in
      let all_at_once_observer = sum_nested_maps_all_at_once map |> Incr.observe in
      let incr_without_update_observer =
        sum_nested_maps_incr map ~use_update:false |> Incr.observe
      in
      let incr_with_update_observer =
        sum_nested_maps_incr map ~use_update:true |> Incr.observe
      in
      stage (fun ~f ->
        Incr.Var.set map_var (f (Incr.Var.value map_var));
        Incr.stabilize ();
        let expect = Incr.Observer.value_exn all_at_once_observer in
        let result_without_update =
          Incr.Observer.value_exn incr_without_update_observer
        in
        let result_with_update = Incr.Observer.value_exn incr_with_update_observer in
        [%test_result: int] result_without_update ~message:"without update" ~expect;
        [%test_result: int] result_with_update ~message:"with update" ~expect;
        if dump
        then
          print_s
            [%sexp
              { sum = (result_without_update : int)
              ; map = (Incr.Var.value map_var : int String.Map.t String.Map.t)
              }])
    ;;

    let%expect_test "manual updates" =
      let initial_map =
        String.Map.of_alist_exn
          (List.map
             ~f:(fun (outer_key, inner_alist) ->
               outer_key, String.Map.of_alist_exn inner_alist)
             [ "a", [ "a", 1 ]; "b", [ "b", 2 ] ])
      in
      let update_and_test = update_and_test ~initial_map ~dump:true |> unstage in
      update_and_test ~f:Fn.id;
      [%expect {|
    ((sum 3)
     (map (
       (a ((a 1)))
       (b ((b 2)))))) |}];
      update_and_test ~f:(fun m ->
        Map.add_exn m ~key:"c" ~data:(String.Map.singleton "c" 4));
      [%expect
        {|
    ((sum 7)
     (map (
       (a ((a 1)))
       (b ((b 2)))
       (c ((c 4)))))) |}];
      update_and_test ~f:(fun m -> Map.remove m "b");
      [%expect {|
    ((sum 5)
     (map (
       (a ((a 1)))
       (c ((c 4)))))) |}];
      update_and_test ~f:(fun m -> Map.set m ~key:"c" ~data:(String.Map.singleton "c" 0));
      [%expect {|
    ((sum 1)
     (map (
       (a ((a 1)))
       (c ((c 0)))))) |}]
    ;;

    module Update = struct
      type t =
        | Remove_random_outer_key
        | Remove_random_inner_key
        | Add_key of
            { outer_key : string
            ; inner_key : string
            ; data : int
            }
        | Set_random_key of { data : int }
      [@@deriving quickcheck]
    end

    let%test_unit "unordered_fold_nested_maps is equivalent to all-at-once" =
      let initial_map = String.Map.empty in
      let random_map_key map = List.random_element (Map.keys map) in
      let apply_to_random_key m ~f = Option.value_map (random_map_key m) ~default:m ~f in
      let set_or_remove_random_value map data =
        apply_to_random_key map ~f:(fun outer_key ->
          Map.update map outer_key ~f:(function
            | None -> assert false
            | Some inner_map ->
              apply_to_random_key inner_map ~f:(fun inner_key ->
                Map.change inner_map inner_key ~f:(fun _ -> data))))
      in
      let f (update : Update.t) map =
        match update with
        | Remove_random_outer_key -> apply_to_random_key map ~f:(Map.remove map)
        | Remove_random_inner_key -> set_or_remove_random_value map None
        | Set_random_key { data } -> set_or_remove_random_value map (Some data)
        | Add_key { outer_key; inner_key; data } ->
          Map.update map outer_key ~f:(fun inner_map ->
            Map.set
              (Option.value inner_map ~default:String.Map.empty)
              ~key:inner_key
              ~data)
      in
      let update_and_test = update_and_test ~initial_map ~dump:false |> unstage in
      update_and_test ~f:Fn.id;
      Quickcheck.test [%quickcheck.generator: Update.t list] ~f:(fun updates ->
        List.iter updates ~f:(fun update -> update_and_test ~f:(f update)))
    ;;
  end)
;;
