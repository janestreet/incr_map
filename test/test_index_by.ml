open Core
open Import

module Odd_or_even = struct
  module T = struct
    type t =
      | Even
      | Odd
    [@@deriving sexp_of, compare, equal]
  end

  include T

  let of_int i = if i % 2 = 0 then Even else Odd

  include Comparable.Make_plain (T)
end

let%expect_test "Simple usage example" =
  let m = String.Map.of_alist_exn [ "a", 1; "b", 2; "c", 3 ] |> Incr.Var.create in
  let ib =
    Incr_map.index_by
      (Incr.Var.watch m)
      ~comparator:(module Odd_or_even)
      ~index:(fun v -> Option.some_if (v >= 0) (Odd_or_even.of_int v))
  in
  let sexp_of_map = [%sexp_of: int String.Map.t Odd_or_even.Map.t] in
  let stabilize_and_get_sexp =
    let obs = Incr.observe ib in
    fun () ->
      Incr.stabilize ();
      sexp_of_map (Incr.Observer.value_exn obs)
  in
  let print_changes f =
    let original = stabilize_and_get_sexp () in
    Incr.Var.set m (f (Incr.Var.value m));
    let updated = stabilize_and_get_sexp () in
    Expect_test_sexp_diff.print_sexp_diff original updated
  in
  print_s (stabilize_and_get_sexp ());
  [%expect
    {|
    ((Even ((b 2)))
     (Odd (
       (a 1)
       (c 3))))
    |}];
  print_changes (fun m -> Map.set m ~key:"a" ~data:2);
  [%expect
    {|
     ((Even        ((Even
       (             (
                  +   (a 2)
        (b 2)))       (b 2)))
      (Odd          (Odd
       (             (
    -   (a 1)
        (c 3))))      (c 3))))
    |}];
  print_changes (fun m -> Map.add_exn m ~key:"d" ~data:57);
  [%expect
    {|
    ((Even ((a 2) (b 2)))   ((Even ((a 2) (b 2)))
     (Odd                    (Odd
      ((c 3)                  ((c 3)
                           +   (d 57)
      )))                     )))
    |}];
  print_changes (fun m -> Map.add_exn m ~key:"e" ~data:(-1));
  [%expect {| |}];
  print_changes (fun m -> Map.set m ~key:"e" ~data:1);
  [%expect
    {|
    ((Even ((a 2) (b 2)))   ((Even ((a 2) (b 2)))
     (Odd                    (Odd
      ((c 3)                  ((c 3)
       (d 57)                  (d 57)
                           +   (e 1)
      )))                     )))
    |}];
  print_changes (fun m -> Map.remove m "b");
  [%expect
    {|
     ((Even                         ((Even
       ((a 2)                         ((a 2)
    -   (b 2)
       ))                             ))
      (Odd ((c 3) (d 57) (e 1))))    (Odd ((c 3) (d 57) (e 1))))
    |}];
  print_changes (fun m -> Map.set m ~key:"a" ~data:5);
  [%expect
    {|
     (                 (
    - (Even ((a 2)))
      (Odd              (Odd
       (                 (
                      +   (a 5)
        (c 3)             (c 3)
        (d 57)            (d 57)
        (e 1))))          (e 1))))
    |}];
  ()
;;

let%test_unit "[Incr_map.index_byi] quickcheck" =
  let index ~key ~data:_ = Some (Odd_or_even.of_int key) in
  let all_at_once map ~comparator ~index =
    Map.to_alist map
    |> List.filter_map ~f:(fun (key, data) ->
      match index ~key ~data with
      | None -> None
      | Some index -> Some (index, (key, data)))
    |> Map.of_alist_multi comparator
    |> Map.map ~f:(Map.of_alist_exn (Map.comparator_s map))
  in
  let var = Incr.Var.create Int.Map.empty in
  let observer =
    Incr_map.index_byi (Incr.Var.watch var) ~comparator:(module Odd_or_even) ~index
    |> Incr.observe
  in
  Quickcheck.test
    (Map_operations.quickcheck_generator String.quickcheck_generator)
    ~f:(fun operations ->
      Map_operations.run_operations operations ~into:var ~after_stabilize:(fun () ->
        [%test_result: string Int.Map.t Odd_or_even.Map.t]
          ~expect:(Incr.Observer.value_exn observer)
          (all_at_once
             (Incr.Var.latest_value var)
             ~comparator:(module Odd_or_even)
             ~index)))
;;
