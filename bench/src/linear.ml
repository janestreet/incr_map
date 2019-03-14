open! Core
open! Import

type sequence_kind =
  | Trivial
  | Recombine
  | Wide
[@@deriving sexp]

let sequence kind n =
  let start = Incr.Var.create 0 in
  let incr =
    match kind with
    | Recombine ->
      Sequence.fold (Sequence.range 0 n) ~init:(Incr.Var.watch start) ~f:(fun incr _i ->
        let a = incr >>| Int.succ in
        let b = incr >>| Int.succ in
        Incr.map2 a b ~f:( + ))
    | Trivial ->
      Sequence.fold (Sequence.range 0 n) ~init:(Incr.Var.watch start) ~f:(fun incr _i ->
        incr >>| Int.succ)
    | Wide ->
      let double l =
        List.concat_map l ~f:(fun i ->
          let a = i >>| Int.succ in
          let b = i >>| Int.succ in
          [ a; b ])
      in
      let spread =
        Sequence.fold
          (Sequence.range 0 n)
          ~init:[ Incr.Var.watch start ]
          ~f:(fun incr_list _ -> double incr_list)
      in
      List.reduce_balanced_exn ~f:(Incr.map2 ~f:( + )) spread
  in
  start, incr
;;

let sequence_raw kind n =
  let input_output =
    lazy
      (let input, output = sequence kind n in
       input, Incr.observe output)
  in
  let run () =
    let input, output = force input_output in
    let open Infix in
    input := !input + 1;
    Incr.stabilize ();
    ignore (Obs.value_exn output : int)
  in
  let name = sprintf "%d linear, %s" n (Sexp.to_string [%sexp (kind : sequence_kind)]) in
  name, run
;;

let sequence_test kind n =
  let name, run = sequence_raw kind n in
  Bench.Test.create ~name run
;;

let sequence_without_change kind n =
  let output =
    lazy
      (let _input, output = sequence kind n in
       Incr.observe output)
  in
  let run () =
    let output = force output in
    Incr.stabilize ();
    ignore (Obs.value_exn output : int)
  in
  let name = sprintf "%d linear (just stabilize)" n in
  Bench.Test.create ~name run
;;

let command () =
  Bench.make_command
    [ sequence_test Recombine 50
    ; sequence_test Trivial 50
    ; sequence_test Wide 5
    ; sequence_test Wide 10
    ; sequence_without_change Recombine 50
    ]
;;

(* You can see here that the cost of a single fire is somewhere between 50 and 150 ns.
   Note that the "recombine" node, however, is 30 nodes per stage.

   {v
┌────────────────────────────┬──────────────┬─────────┬──────────┬──────────┬────────────┐
│ Name                       │     Time/Run │ mWd/Run │ mjWd/Run │ Prom/Run │ Percentage │
├────────────────────────────┼──────────────┼─────────┼──────────┼──────────┼────────────┤
│ 50 linear, Recombine       │  13_952.40ns │         │          │          │      4.34% │
│ 50 linear, Trivial         │   2_202.29ns │         │          │          │      0.69% │
│ 5 linear, Wide             │   9_319.77ns │         │          │          │      2.90% │
│ 10 linear, Wide            │ 321_275.45ns │ -14.38w │    8.65w │    8.65w │    100.00% │
│ 50 linear (just stabilize) │      67.91ns │         │          │          │      0.02% │
└────────────────────────────┴──────────────┴─────────┴──────────┴──────────┴────────────┘
   v} *)

let%expect_test "stats" =
  let stats = unstage (Stats.reporter ()) in
  stats ();
  [%expect
    {|
    ((recomputed  0)
     (changed     0)
     (created     0)
     (invalidated 0)) |}];
  let _name, run = sequence_raw Recombine 50 in
  stats ();
  [%expect
    {|
    ((recomputed  0)
     (changed     0)
     (created     0)
     (invalidated 0)) |}];
  run ();
  stats ();
  [%expect
    {|
    ((recomputed  151)
     (changed     151)
     (created     151)
     (invalidated 0)) |}];
  run ();
  stats ();
  [%expect
    {|
    ((recomputed  151)
     (changed     151)
     (created     0)
     (invalidated 0)) |}];
  run ();
  stats ();
  [%expect
    {|
    ((recomputed  151)
     (changed     151)
     (created     0)
     (invalidated 0)) |}]
;;
