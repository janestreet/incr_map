(* This code tests that generic functions can operate on [Incr_map.Make] values. *)

open! Core_kernel
open! Import
module I = Incremental.Make ()
module M = Incr_map.Make (I)

let%expect_test _ =
  let i = I.return (Map.singleton (module Int) 0 "hello") in
  let (_ : _) = M.mapi i ~f:(fun ~key:_ ~data -> data) in
  let (_ : _) = Incr_map.mapi i ~f:(fun ~key:_ ~data -> data) in
  let lookup = M.Lookup.create i ~comparator:(Map.comparator Int.Map.empty) in
  let (_ : _) = M.Lookup.find lookup 0 in
  let (_ : _) = Incr_map.Lookup.find lookup 0 in
  [%expect {||}]
;;
