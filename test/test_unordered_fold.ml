open! Core.Std
open! Import

(* version of unorderd fold which will fail if there isn't a match. *)
let unordered_fold (type a) ~data_equal m ~(init:a) ~f ~f_inverse =
  let a = Incr.Map.unordered_fold ~data_equal m ~init ~f ~f_inverse in
  let b =
    let%map m = m in
    Map.fold ~init ~f m
  in
  let%map a = a and b = b in
  require [%here] (data_equal a b);
  a

let%expect_test _ =
  let map = Incr.Var.create (String.Map.of_alist_exn ["a", 1; "b", 2]) in
  let map_o = Incr.observe (Incr.Var.watch map) in
  let sum_o =
    unordered_fold (Incr.Var.watch map)
      ~data_equal:Int.equal ~init:0
      ~f:(fun ~key:_ ~data:v acc -> acc + v)
      ~f_inverse:(fun ~key:_ ~data:v acc -> acc - v)
    |> Incr.observe
  in
  let dump () =
    Incr.stabilize ();
    let value = Incr.Observer.value_exn in
    Sexp.to_string_hum ([%sexp_of: (int * int String.Map.t)] (value sum_o, value map_o))
    |> print_endline
  in
  let change f = Incr.Var.set map (f (Incr.Var.value map)) in
  dump (); [%expect {| (3 ((a 1) (b 2))) |}];
  change (fun m -> Map.add m ~key:"c" ~data:4);
  dump (); [%expect {| (7 ((a 1) (b 2) (c 4))) |}];
  change (fun m -> Map.remove m "b");
  dump (); [%expect {| (5 ((a 1) (c 4))) |}];
  change (fun m -> Map.add m ~key:"c" ~data:0);
  dump (); [%expect {| (1 ((a 1) (c 0))) |}];
;;

