open! Core
open! Import

let%test_unit "rekey random test" =
  Quickcheck.test
    ~sexp_of:[%sexp_of: (int, int) Map_operations.t list]
    (Map_operations.quickcheck_generator Int.quickcheck_generator)
    ~f:(fun operations ->
      let m = Incr.Var.create Int.Map.empty in
      let watch_m = Incr.Var.watch m in
      let fast =
        Incr_map.rekey
          ~comparator:(module String)
          watch_m
          ~f:(fun ~key ~data:_ -> Int.to_string key)
      in
      let slow =
        let%map watch_m = watch_m in
        watch_m
        |> Map.to_alist
        |> List.map ~f:(fun (k, v) -> Int.to_string k, v)
        |> Map.of_alist_exn (module String)
      in
      let fast_obs = Incr.observe fast in
      let slow_obs = Incr.observe slow in
      Map_operations.run_operations operations ~into:m ~after_stabilize:(fun () ->
        [%test_result: int String.Map.t]
          ~expect:(Incr.Observer.value_exn fast_obs)
          (Incr.Observer.value_exn slow_obs)))
;;
