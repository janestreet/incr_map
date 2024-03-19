open Core
open Import

let%expect_test "unzip_mapi' dropping one map" =
  let m = String.Map.of_alist_exn [ "foo", 3; "bar", 10; "snoo", 5 ] |> Incr.Var.create in
  let a, _ =
    Incr.Var.watch m |> Incr.Map.unzip_mapi' ~f:(fun ~key:_ ~data -> data, data)
  in
  let observer = Incr.observe a in
  let dump () =
    Incr.stabilize ();
    observer |> Incr.Observer.value_exn |> [%sexp_of: int String.Map.t] |> print_s
  in
  let change f =
    Incr.Var.set m (f (Incr.Var.value m));
    dump ()
  in
  dump ();
  [%expect {|
    ((bar  10)
     (foo  3)
     (snoo 5))
    |}];
  change (fun m -> Map.set m ~key:"foo" ~data:9);
  [%expect {|
    ((bar  10)
     (foo  9)
     (snoo 5))
    |}];
  change (fun m -> Map.set m ~key:"bar" ~data:1);
  [%expect {|
    ((bar  1)
     (foo  9)
     (snoo 5))
    |}];
  change (fun m -> Map.remove m "snoo");
  [%expect {|
    ((bar 1)
     (foo 9))
    |}]
;;
