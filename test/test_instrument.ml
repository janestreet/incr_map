open Core
open Import

let instrumentation =
  { Incr_map.Instrumentation.f =
      (fun f ->
        print_endline "starting!";
        let r = f () in
        print_endline "finishing!";
        r)
  }
;;

let%expect_test _ =
  let map = Incr.Var.create (String.Map.of_alist_exn [ "a", 1; "b", 2 ]) in
  let map_o = Incr.observe (Incr.Var.watch map) in
  let sum_o =
    Incr_map.unordered_fold
      ~instrumentation
      (Incr.Var.watch map)
      ~data_equal:Int.equal
      ~init:0
      ~add:(fun ~key:_ ~data:v acc -> acc + v)
      ~remove:(fun ~key:_ ~data:v acc -> acc - v)
    |> Incr.observe
  in
  let dump () =
    Incr.stabilize ();
    let value = Incr.Observer.value_exn in
    Sexp.to_string_hum ([%sexp_of: int * int String.Map.t] (value sum_o, value map_o))
    |> print_endline
  in
  let change f = Incr.Var.set map (f (Incr.Var.value map)) in
  dump ();
  [%expect
    {|
    starting!
    finishing!
    (3 ((a 1) (b 2)))
    |}];
  change (fun m -> Map.set m ~key:"c" ~data:4);
  dump ();
  [%expect
    {|
    starting!
    finishing!
    (7 ((a 1) (b 2) (c 4)))
    |}];
  (* This doesn't trigger the instrumentation because of cutoff *)
  change (fun m -> m);
  dump ();
  [%expect {| (7 ((a 1) (b 2) (c 4))) |}]
;;
