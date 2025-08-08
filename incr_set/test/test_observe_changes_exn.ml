open Core

let on_add x = [%message "add" (x : int)] |> Core.print_s
let on_remove x = [%message "remove" (x : int)] |> Core.print_s

let%expect_test "basic" =
  let module Incr = Incremental.Make () in
  let m = Incr.Var.create Int.Set.empty in
  Incr_set.observe_changes_exn (Incr.Var.watch m) ~on_add ~on_remove;
  let set_to list =
    Incr.Var.set m (Int.Set.of_list list);
    Incr.stabilize ()
  in
  set_to [ 1; 2; 3 ];
  [%expect
    {|
    (add (x 1))
    (add (x 2))
    (add (x 3))
    |}];
  set_to [ 2; 3; 4 ];
  [%expect
    {|
    (remove (x 1))
    (add (x 4))
    |}]
;;

let%expect_test "banned inside bind" =
  let module Incr = Incremental.Make () in
  let open Incr.Let_syntax in
  let b = Incr.Var.create true in
  let m = Incr.Var.create Int.Set.empty in
  let result =
    if%bind Incr.Var.watch b
    then (
      Incr_set.observe_changes_exn (Incr.Var.watch m) ~on_add ~on_remove;
      return ())
    else return ()
  in
  let (_ : unit Incr.Observer.t) = Incr.observe result in
  Expect_test_helpers_core.require_does_raise (fun () -> Incr.stabilize ());
  [%expect
    {|
    (Failure
     "[Incr_set.observe_changes_exn] called in scope that is not top-level")
    |}]
;;
