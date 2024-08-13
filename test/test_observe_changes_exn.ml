open! Core

let%expect_test "basic" =
  let module Incr = Incremental.Make () in
  let m = Incr.Var.create Int.Map.empty in
  let f update =
    [%sexp_of: (int, int) Map.Symmetric_diff_element.t] update |> Core.print_s
  in
  Incr_map.observe_changes_exn (Incr.Var.watch m) ~f;
  let set_to alist =
    Incr.Var.set m (Int.Map.of_alist_exn alist);
    Incr.stabilize ()
  in
  set_to [ 3, 0; 4, 0; 5, 0 ];
  [%expect
    {|
    (3 (Right 0))
    (4 (Right 0))
    (5 (Right 0))
    |}];
  set_to [ 3, 1; 5, 0 ];
  [%expect
    {|
    (3 (Unequal (0 1)))
    (4 (Left 0))
    |}]
;;

let%expect_test "banned inside bind" =
  let module Incr = Incremental.Make () in
  let open Incr.Let_syntax in
  let b = Incr.Var.create true in
  let m = Incr.Var.create Int.Map.empty in
  let result =
    if%bind Incr.Var.watch b
    then (
      let f update =
        [%sexp_of: (int, int) Map.Symmetric_diff_element.t] update |> Core.print_s
      in
      Incr_map.observe_changes_exn (Incr.Var.watch m) ~f;
      return ())
    else return ()
  in
  let (_ : unit Incr.Observer.t) = Incr.observe result in
  Expect_test_helpers_core.require_does_raise (fun () -> Incr.stabilize ());
  [%expect
    {|
    (Failure
     "[Incr_map.observe_changes_exn] called in scope that is not top-level")
    |}]
;;
