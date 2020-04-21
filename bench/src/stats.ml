open! Core
open Import

type t =
  { recomputed : int
  ; changed : int
  ; created : int
  }
[@@deriving sexp]

let diff t1 t2 =
  { recomputed = t1.recomputed - t2.recomputed
  ; changed = t1.changed - t2.changed
  ; created = t1.created - t2.created
  }
;;

let snap () =
  { recomputed = Incr.State.num_nodes_recomputed Incr.State.t
  ; changed = Incr.State.num_nodes_changed Incr.State.t
  ; created = Incr.State.num_nodes_created Incr.State.t
  }
;;

let reporter () =
  let open Expect_test_helpers_core in
  let old_stats = ref (snap ()) in
  let report () =
    let stats = snap () in
    print_s [%sexp (diff stats !old_stats : t)];
    old_stats := stats
  in
  stage report
;;
