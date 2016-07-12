open Core.Std
open Import

(** version of filter_mapi that tests the real implementation against a simple, all-at
    once one, and fails if the two implementations don't match.*)
let filter_mapi ~data_equal m ~f =
  let a = Incr.Map.filter_mapi ~data_equal m ~f in
  let b =
    let%map m = m in
    Map.filter_mapi ~f:(fun ~key ~data -> f ~key ~data) m
  in
  let%map a = a and b = b in
  require [%here] (Map.equal data_equal a b);
  a
;;

let%expect_test "simple filter_mapi" =
  let m =
    String.Map.of_alist_exn ["foo",3; "bar",10; "snoo",5]
    |> Incr.Var.create
  in
  let fm = filter_mapi ~data_equal:Int.equal (Incr.Var.watch m) ~f:(fun ~key:_ ~data:x ->
    let y = x * x in
    if y > 10 then Some y else None)
           |> Incr.observe
  in
  let dump () =
    Incr.stabilize ();
    ([%sexp_of: int String.Map.t * string * int String.Map.t]
       (Incr.Var.value m, "->", Incr.Observer.value_exn fm))
    |> Sexp.to_string_hum |> print_endline
  in
  let change f = Incr.Var.set m (f (Incr.Var.value m)); dump ()in
  dump ();
  [%expect {| (((bar 10) (foo 3) (snoo 5)) -> ((bar 100) (snoo 25))) |}];
  change (fun m -> Map.add m ~key:"foo" ~data:9);
  [%expect {| (((bar 10) (foo 9) (snoo 5)) -> ((bar 100) (foo 81) (snoo 25))) |}];
  change (fun m -> Map.add m ~key:"bar" ~data:1);
  [%expect {| (((bar 1) (foo 9) (snoo 5)) -> ((foo 81) (snoo 25))) |}];
  change (fun m -> Map.remove m "snoo");
  [%expect {| (((bar 1) (foo 9)) -> ((foo 81))) |}];
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
      let length = match keys_size with
        | Some n -> `Exactly n
        | None -> `At_least 5
      in
      List.gen' ~length Int.gen
      >>| List.dedup ~compare:Int.compare
    in
    let key_gen = Quickcheck.Generator.of_list keys in
    List.gen' ?length:operations (Quickcheck.Generator.weighted_union [
      1., return Stabilize;
      4., (let%map key = key_gen in Remove key);
      10., (let%map key = key_gen and data = data_gen in Add (key, data));
    ])
  ;;

  let gen data_gen = gen' data_gen

  let run_operations operations ~into:var ~after_stabilize =
    List.fold operations ~init:Int.Map.empty ~f:(fun map oper ->
      match oper with
      | Add (key, data) -> Map.add map ~key ~data
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
    (Map_operations.gen Int.gen)
    ~f:(fun operations ->
      let m = Incr.Var.create Int.Map.empty in
      let watch_m = Incr.Var.watch m
      and f ~key ~data =
        let y = data * data in
        Option.some_if (key + y > 33) y
      in
      let incr_filter_mapi =
        Incr.Map.filter_mapi watch_m ~data_equal:Int.equal ~f
        |> Incr.observe
      and slow_filter_mapi =
        Incr.map watch_m ~f:(Map.filter_mapi ~f)
        |> Incr.observe
      in
      Map_operations.run_operations operations ~into:m ~after_stabilize:(fun () ->
        [%test_result: int Int.Map.t]
          ~expect:(Incr.Observer.value_exn slow_filter_mapi)
          (Incr.Observer.value_exn incr_filter_mapi));
      Incr.Observer.disallow_future_use incr_filter_mapi;
      Incr.Observer.disallow_future_use slow_filter_mapi);
;;

let%bench_module "filter_mapi" = (
  module struct
    let test_data ~size ~operations =
      (* It is important this is deterministic *)
      Quickcheck.random_value
        ~seed:(`Deterministic (
          sprintf "%i-%i-%i-hello world, do not forget your towel"
            42 size operations))
        (Map_operations.gen' Int.gen
           ~keys_size:size
           ~operations:(`Exactly operations))
    ;;

    let benchmark_filter_mapi filter_mapi ~operations =
      let var =
        Incr.Var.create Int.Map.empty
      and test_map_fn ~key ~data =
        let y = data * data in
        Option.some_if (y + key > 33) y
      in
      let mapped = Incr.observe (
        filter_mapi ?data_equal:(Some Int.equal) (Incr.Var.watch var) ~f:test_map_fn)
      in
      Map_operations.run_operations operations ~into:var ~after_stabilize:(fun () ->
        ignore (Incr.Observer.value_exn mapped));
      Incr.Observer.disallow_future_use mapped;
    ;;

    let%bench_fun "random-ops" [@indexed operations = [5000; 10000; 100000]] =
      let operations = test_data ~size:(operations / 100) ~operations in
      fun () -> benchmark_filter_mapi Incr.Map.filter_mapi ~operations
    ;;
end)

let%test_unit "filter_mapi' should not throw on empty map" =
  let observer = Incr.observe (
    Incr.Map.filter_mapi'
      ~f:(fun ~key:_ ~data -> Incr.map ~f:Option.return data)
      (Incr.Var.watch (Incr.Var.create Int.Map.empty)))
  in
  Incr.stabilize ();
  Incr.Observer.disallow_future_use observer;
;;

type data = F of bool | G of bool | H of bool
[@@deriving sexp, compare]

let%expect_test "check filter_mapi' against actual filter_mapi" =
  let input_var = Incr.Var.create Int.Map.empty in
  let f ~key x = match key % 3 with
    | 0 -> Some (F x)
    | 1 -> Some (G x)
    | 2 -> Option.some_if x (H x)
    | _ -> None
  in
  let watch_input = Incr.Var.watch input_var in
  let via_incr =
    Incr.Map.filter_mapi' watch_input
      ~f:(fun ~key ~data -> Incr.map data ~f:(f ~key))
    |> Incr.observe
  and via_map =
    Incr.map watch_input
      ~f:(fun map -> Map.filter_mapi map ~f:(fun ~key ~data -> f ~key data))
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
  test_with [1, true;];
  [%expect {| ((1 (G true))) |}];
  test_with [1, false;];
  [%expect {| ((1 (G false))) |}];
  test_with [1, false; 2, false;];
  [%expect {| ((1 (G false))) |}];
  test_with [1, false; 2, true;];
  [%expect {|
        ((1 (G false))
         (2 (H true))) |}];
  test_with [1, false; 2, false;];
  [%expect {| ((1 (G false))) |}];
  test_with [1, false; 2, true;];
  [%expect {|
        ((1 (G false))
         (2 (H true))) |}];
  test_with [1, true; 2, true; 3, false;];
  [%expect {|
        ((1 (G true))
         (2 (H true))
         (3 (F false))) |}];
  test_with [1, true; 2, true;];
  [%expect {|
        ((1 (G true))
         (2 (H true))) |}];
;;

