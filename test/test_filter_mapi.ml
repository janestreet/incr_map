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

