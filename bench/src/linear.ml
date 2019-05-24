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
  let input, output =
    let input, output = sequence kind n in
    input, Incr.observe output
  in
  fun () ->
    let open Infix in
    input := !input + 1;
    Incr.stabilize ();
    ignore (Obs.value_exn output : int)
;;

let sequence_without_change kind n =
  let output =
    let _input, output = sequence kind n in
    Incr.observe output
  in
  fun () ->
    Incr.stabilize ();
    ignore (Obs.value_exn output : int)
;;

let%bench_fun "Recombine 50" = sequence_raw Recombine 50
let%bench_fun "Trivial 50" = sequence_raw Trivial 50
let%bench_fun "Wide 5" = sequence_raw Wide 5
let%bench_fun "Wide 10" = sequence_raw Wide 10
let%bench_fun "50 (just stabilize)" = sequence_without_change Recombine 50

(*
   Each iteration of "trivial 50" updates 50 incremental nodes.
   Each iteration of "recombine 50" updates ~150 nodes.
   Each iteration of "wide K" updates about 3 * 2^K nodes.

   Dividing out times per run shows that each node update takes somewhere between 15 and
   50 nanoseconds.

   {v
┌─────────────────────────────────┬──────────────┬─────────┬────────────┐
│ Name                            │     Time/Run │ mWd/Run │ Percentage │
├─────────────────────────────────┼──────────────┼─────────┼────────────┤
│ [linear.ml] Recombine 50        │   6_925.04ns │         │      3.96% │
│ [linear.ml] Trivial 50          │     729.76ns │         │      0.42% │
│ [linear.ml] Wide 5              │   5_059.74ns │         │      2.90% │
│ [linear.ml] Wide 10             │ 174_702.44ns │  -0.81w │    100.00% │
│ [linear.ml] 50 (just stabilize) │      36.94ns │         │      0.02% │
└─────────────────────────────────┴──────────────┴─────────┴────────────┘

   v} *)

let%expect_test "stats" =
  let stats = unstage (Stats.reporter ()) in
  stats ();
  [%expect {|
    ((recomputed 0)
     (changed    0)
     (created    0)) |}];
  let run = sequence_raw Recombine 50 in
  stats ();
  [%expect {|
    ((recomputed 0)
     (changed    0)
     (created    151)) |}];
  run ();
  stats ();
  [%expect {|
    ((recomputed 151)
     (changed    151)
     (created    0)) |}];
  run ();
  stats ();
  [%expect {|
    ((recomputed 151)
     (changed    151)
     (created    0)) |}];
  run ();
  stats ();
  [%expect {|
    ((recomputed 151)
     (changed    151)
     (created    0)) |}]
;;
