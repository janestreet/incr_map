open! Core
open Import

(** Test cost of building up (and stabilizing along the way) some derived incr node of a
    map. *)

let input =
  lazy
    (let size = 100_000 in
     Sequence.fold (Sequence.init size ~f:Int.succ) ~init:Int.Map.empty ~f:(fun map i ->
       Map.set map ~key:i ~data:i))
;;

module M (S : sig
    val name : string
    val f : int Int.Map.t Incr.t -> int Incr.t
  end) =
struct
  let name = S.name
  let f = S.f

  let%bench_module (""[@name_suffix name]) =
    (module struct
      (* Build a map from scratch, one [Map.set] per element, stabilizing along the way. *)
      let%bench_fun ("make"[@indexed size = [ 1000; 100_000 ]]) =
        fun () ->
          let open Infix in
          let input = Incr.Var.create Int.Map.empty in
          let output = f (Incr.Var.watch input) |> Incr.observe in
          for i = 1 to size do
            input := Map.set !input ~key:i ~data:i;
            Incr.stabilize ();
            assert (Incr.Observer.value_exn output = i)
          done
      ;;

      (* Take a map as input, update one key (1, specifically), and stabilize *)
      let%bench_fun "update" =
        let input = force input in
        let size = Map.length input in
        let input = Incr.Var.create input in
        let output = f (Incr.Var.watch input) |> Incr.observe in
        Incr.stabilize ();
        fun () ->
          let open Infix in
          input
          := Map.update !input 1 ~f:(function
            | None -> 0
            | Some i -> i + 1);
          Incr.stabilize ();
          assert (Incr.Observer.value_exn output = size)
      ;;

      (* Add, stabilize, remove, to existing input map. *)
      let%bench_fun "add/remove" =
        let input = force input in
        let unused = Map.max_elt_exn input |> fst |> Int.succ in
        let size = Map.length input in
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
      ;;
    end)
  ;;
end

module M1 = M (struct
    let name = "trivial"
    let f map = map >>| Map.length
  end)

module M2 = M (struct
    let name = "mapi"
    let f map = Incr_map.mapi map ~f:(fun ~key ~data -> key + data) >>| Map.length
  end)

module M3 = M (struct
    let name = "mapi'"

    let f map =
      Incr_map.mapi' map ~f:(fun ~key ~data ->
        let%map data = data in
        key + data)
      >>| Map.length
    ;;
  end)
