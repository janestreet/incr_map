open! Core
open Import

(** Test cost of building up (and stabilizing along the way) some derived incr node of a
    map. *)

(** build a map from scratch, one [Map.set] per element, stabilizing along the way. *)
let make_raw ~name ~size ~(f : int Int.Map.t Incr.t -> int Incr.t) =
  let name = sprintf "create %s (%d)" name size in
  let run () =
    let open Infix in
    let input = Incr.Var.create Int.Map.empty in
    let output = f (Incr.Var.watch input) |> Incr.observe in
    for i = 1 to size do
      input := Map.set !input ~key:i ~data:i;
      Incr.stabilize ();
      assert (Incr.Observer.value_exn output = i)
    done
  in
  Bench.Test.create ~name run
;;

(** Taking a map as input, update one key (1, specifically), and stabilize *)
let update_raw ~name ~input ~(f : int Int.Map.t Incr.t -> int Incr.t) =
  let size = Map.length input in
  let name = sprintf "update %s (%d)" name size in
  let run `init =
    let input = Incr.Var.create input in
    let output = f (Incr.Var.watch input) |> Incr.observe in
    Incr.stabilize ();
    fun () ->
      let open Infix in
      input :=
        Map.update !input 1 ~f:(function
          | None -> 0
          | Some i -> i + 1);
      Incr.stabilize ();
      assert (Incr.Observer.value_exn output = size)
  in
  Bench.Test.create_with_initialization ~name run
;;

(** Add, stabilize, remove, to existing input map. *)
let add_remove_raw ~name ~input ~f =
  let unused = Map.max_elt_exn input |> fst |> Int.succ in
  let size = Map.length input in
  let name = sprintf "add/remove %s (%d)" name size in
  let run `init =
    let input = Incr.Var.create input in
    let output = f (Incr.Var.watch input) |> Incr.observe in
    Incr.stabilize ();
    let size' = size + 1 in
    fun () ->
      let open Infix in
      input := Map.add_exn !input ~key:unused ~data:0;
      Incr.stabilize ();
      assert (Incr.Observer.value_exn output = size');
      input := Map.remove !input unused;
      Incr.stabilize ();
      assert (Incr.Observer.value_exn output = size)
  in
  Bench.Test.create_with_initialization ~name run
;;

let trivial map = map >>| Map.length
let mapi map = Incr_map.mapi map ~f:(fun ~key ~data -> key + data) |> trivial

let mapi' map =
  Incr_map.mapi' map ~f:(fun ~key ~data ->
    let%map data = data in
    key + data)
  |> trivial
;;

let make_trivial = make_raw ~name:"trivial" ~f:trivial
let make = make_raw ~name:"mapi" ~f:mapi
let make' = make_raw ~name:"mapi'" ~f:mapi'
let update_trivial = update_raw ~name:"trivial" ~f:trivial
let update = update_raw ~name:"mapi" ~f:mapi
let update' = update_raw ~name:"mapi'" ~f:mapi'
let add_remove_trivial = add_remove_raw ~name:"trivial" ~f:trivial
let add_remove = add_remove_raw ~name:"mapi" ~f:mapi
let add_remove' = add_remove_raw ~name:"mapi'" ~f:mapi'

let input =
  let size = 100_000 in
  Sequence.fold (Sequence.init size ~f:Int.succ) ~init:Int.Map.empty ~f:(fun map i ->
    Map.set map ~key:i ~data:i)
;;

let command () =
  Bench.make_command
    [ make_trivial ~size:1000
    ; make ~size:1000
    ; make' ~size:1000
    ; make_trivial ~size:100_000
    ; make ~size:100_000
    ; make' ~size:100_000
    ; update_trivial ~input
    ; update ~input
    ; update' ~input
    ; add_remove_trivial ~input
    ; add_remove ~input
    ; add_remove' ~input
    ]
;;
