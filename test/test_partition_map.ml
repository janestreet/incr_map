open! Core
open! Import

module%test _ = struct
  let f ~key:_ ~data =
    match data mod 2 = 0 with
    | true -> First (sprintf "N=%d" data)
    | false -> Second data
  ;;

  let%expect_test "manual updates" =
    let var = [ "a", 1; "b", 2 ] |> String.Map.of_alist_exn |> Incr.Var.create in
    let observer = Incr.observe (Incr_map.partition_mapi ~f (Incr.Var.watch var)) in
    let update_and_test ~f =
      Incr.Var.replace var ~f;
      Incr.stabilize ();
      print_s
        [%sexp
          (Incr.Observer.value_exn observer : string String.Map.t * int String.Map.t)]
    in
    update_and_test ~f:Fn.id;
    [%expect
      {|
      (((b N=2))
       ((a 1)))
      |}];
    update_and_test ~f:(fun m -> Map.add_exn m ~key:"c" ~data:3);
    [%expect
      {|
      (((b N=2))
       ((a 1)
        (c 3)))
      |}];
    update_and_test ~f:(fun m -> Map.remove m "b");
    [%expect
      {|
      (()
       ((a 1)
        (c 3)))
      |}];
    update_and_test ~f:(fun m -> Map.set m ~key:"c" ~data:100);
    [%expect
      {|
      (((c N=100))
       ((a 1)))
      |}];
    update_and_test ~f:(fun m -> Map.set m ~key:"d" ~data:11);
    [%expect
      {|
      (((c N=100))
       ((a 1)
        (d 11)))
      |}]
  ;;

  let%test_unit "randomized map changes" =
    let var = Incr.Var.create Int.Map.empty in
    let observer =
      Incremental.observe (Incr_map.partition_mapi (Incr.Var.watch var) ~f)
    in
    Quickcheck.test
      (Map_operations.quickcheck_generator [%quickcheck.generator: int])
      ~f:(fun operations ->
        Map_operations.run_operations operations ~into:var ~after_stabilize:(fun () ->
          [%test_result: string Int.Map.t * int Int.Map.t]
            ~expect:(Map.partition_mapi (Incr.Var.latest_value var) ~f)
            (Incremental.Observer.value_exn observer)))
  ;;
end

module%test [@name "partition_mapi'"] _ = struct
  let f ~key:_ ~data =
    match data mod 2 = 0 with
    | true -> First (sprintf "N=%d" data)
    | false -> Second data
  ;;

  let incr_f ~key ~data =
    let%map data in
    f ~key ~data
  ;;

  let%expect_test "manual updates" =
    let var = [ "a", 1; "b", 2 ] |> String.Map.of_alist_exn |> Incr.Var.create in
    let observer =
      Incr.observe (Incr_map.partition_mapi' ~f:incr_f (Incr.Var.watch var))
    in
    let update_and_test ~f =
      Incr.Var.replace var ~f;
      Incr.stabilize ();
      print_s
        [%sexp
          (Incr.Observer.value_exn observer : string String.Map.t * int String.Map.t)]
    in
    update_and_test ~f:Fn.id;
    [%expect
      {|
      (((b N=2))
       ((a 1)))
      |}];
    update_and_test ~f:(fun m -> Map.add_exn m ~key:"c" ~data:3);
    [%expect
      {|
      (((b N=2))
       ((a 1)
        (c 3)))
      |}];
    update_and_test ~f:(fun m -> Map.remove m "b");
    [%expect
      {|
      (()
       ((a 1)
        (c 3)))
      |}];
    update_and_test ~f:(fun m -> Map.set m ~key:"c" ~data:100);
    [%expect
      {|
      (((c N=100))
       ((a 1)))
      |}];
    update_and_test ~f:(fun m -> Map.set m ~key:"d" ~data:11);
    [%expect
      {|
      (((c N=100))
       ((a 1)
        (d 11)))
      |}]
  ;;

  let%test_unit "randomized map changes" =
    let var = Incr.Var.create Int.Map.empty in
    let observer =
      Incremental.observe (Incr_map.partition_mapi' (Incr.Var.watch var) ~f:incr_f)
    in
    Quickcheck.test
      (Map_operations.quickcheck_generator [%quickcheck.generator: int])
      ~f:(fun operations ->
        Map_operations.run_operations operations ~into:var ~after_stabilize:(fun () ->
          [%test_result: string Int.Map.t * int Int.Map.t]
            ~expect:(Map.partition_mapi (Incr.Var.latest_value var) ~f)
            (Incremental.Observer.value_exn observer)))
  ;;
end
