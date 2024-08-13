open! Core
module Incr = Incr_map_test.Import.Incr
module U = Incr_map_test.Rand_map_helper

module Out = struct
  type t = (int * float) list [@@deriving compare, sexp, equal]
end

let check_invariants input_map derived_map =
  let input_list = Map.to_alist input_map in
  let derived_list = Map.data derived_map in
  Expect_test_helpers_base.require_compare_equal (module Out) input_list derived_list;
  assert ([%equal: Out.t] input_list derived_list);
  let (_ : _ String.Map.t) =
    (* The keys of the derived map should serialized to unique values. If that
       is not the case, then [of_alist_exn] will raise. *)
    Map.to_alist derived_map
    |> List.map ~f:(fun (key, data) -> Opaque_map.Key.to_string key, data)
    |> String.Map.of_alist_exn
  in
  ()
;;

let run_test ~size_of_initial_map ~iterations =
  let input_map = Incr.Var.create (U.init_rand_map ~from:0 ~to_:size_of_initial_map) in
  let derived_map =
    Opaque_map.erase_key_incrementally
      ~get:(fun ~key ~data -> key, data)
      (Incr.Var.watch input_map)
  in
  let both = Incr.both (Incr.Var.watch input_map) derived_map in
  let both = Incr.observe both in
  let test () =
    Incr.stabilize ();
    let input_map, derived_map = Incr.Observer.value_exn both in
    check_invariants input_map derived_map
  in
  for _ = 0 to iterations do
    if Float.( < ) (U.rand ()) 0.25
    then test ()
    else Incr.Var.set input_map (U.rand_modify_map (Incr.Var.value input_map))
  done
;;

let%expect_test _ = run_test ~size_of_initial_map:0 ~iterations:1000
let%expect_test _ = run_test ~size_of_initial_map:100 ~iterations:1000
let%expect_test _ = run_test ~size_of_initial_map:1000 ~iterations:1000
let%expect_test _ = run_test ~size_of_initial_map:1000 ~iterations:10000

let%expect_test _ =
  let size = 1500 in
  let input_map = Incr.Var.create (Int.Map.of_alist_exn [ -1, 0.0; size + 1, 0.0 ]) in
  let derived_map =
    Opaque_map.erase_key_incrementally
      ~get:(fun ~key ~data -> key, data)
      (Incr.Var.watch input_map)
  in
  let both = Incr.both (Incr.Var.watch input_map) derived_map in
  let both = Incr.observe both in
  let test () =
    Incr.stabilize ();
    let input_map, derived_map = Incr.Observer.value_exn both in
    check_invariants input_map derived_map
  in
  for i = 0 to size - 1 do
    Incr.Var.set input_map (Map.set (Incr.Var.value input_map) ~key:i ~data:0.0);
    test ()
  done
;;

(* This is a regression-test for [Opaque_map] that demonstrates changes to an
   input map where the map never gets larger than size 3, but the denomonator for the
   bignum assigned to the key for the middle-most row grows explosively. *)
let%expect_test _ =
  let needle = ref (Bignum.of_float_decimal 0.5) in
  let input_map =
    Incr.Var.create
      (Bignum.Map.of_alist_exn
         [ Bignum.of_int (-1), (); !needle, (); Bignum.of_int 1, () ])
  in
  let derived_map =
    Opaque_map.erase_key_incrementally
      ~get:(fun ~key:_ ~data:_ -> ())
      (Incr.Var.watch input_map)
  in
  let observer = Incr.observe derived_map in
  for _ = 0 to 1000 do
    Incr.Var.replace input_map ~f:(fun map ->
      let new_needle = Bignum.(!needle / (one + one)) in
      let map = Map.remove map !needle in
      let map = Map.add_exn map ~key:new_needle ~data:() in
      needle := new_needle;
      map);
    Incr.stabilize ()
  done;
  observer |> Incr.Observer.value_exn |> [%sexp_of: unit Opaque_map.Key.Map.t] |> print_s;
  [%expect {| ((0 ()) (100 ()) (200 ())) |}]
;;

(* This test is a regression-test for pathological inputs to incr_map_erase_key which
   cause the bignum key to grow explosively.  This one utilizes two pointers which jump
   over one another repeatedly, each time multiplying the denomonator by two. *)
let%expect_test _ =
  let needle = ref (Bignum.of_float_decimal 0.5) in
  let needle' = ref (Bignum.of_float_decimal 0.6) in
  let input_map =
    Incr.Var.create
      (Bignum.Map.of_alist_exn
         [ Bignum.of_int (-1), (); !needle, (); !needle', (); Bignum.of_int 1, () ])
  in
  let derived_map =
    Opaque_map.erase_key_incrementally
      ~get:(fun ~key:_ ~data:_ -> ())
      (Incr.Var.watch input_map)
  in
  let observer = Incr.observe derived_map in
  for _ = 0 to 1000 do
    Incr.Var.replace input_map ~f:(fun map ->
      let new_needle = Bignum.(!needle / (one + one)) in
      let map = Map.remove map !needle' in
      let map = Map.add_exn map ~key:new_needle ~data:() in
      needle' := !needle;
      needle := new_needle;
      map);
    Incr.stabilize ()
  done;
  observer |> Incr.Observer.value_exn |> [%sexp_of: unit Opaque_map.Key.Map.t] |> print_s;
  [%expect {| ((0 ()) (0.0625 ()) (0.125 ()) (300 ())) |}]
;;

let%expect_test "adding elements before and after existing elements" =
  let test inputs =
    let input_map = Incr.Var.create (Int.Map.of_alist_exn [ 0, () ]) in
    let derived_map =
      Opaque_map.erase_key_incrementally
        ~get:(fun ~key ~data:() -> key)
        (Incr.Var.watch input_map)
    in
    let observer = Incr.observe derived_map in
    Incr.stabilize ();
    Incr.Var.replace input_map ~f:(fun map ->
      List.fold inputs ~init:map ~f:(fun map key -> Map.set map ~key ~data:()));
    Incr.stabilize ();
    observer
    |> Incr.Observer.value_exn
    |> Map.iteri ~f:(fun ~key ~data ->
      print_s [%sexp (key : Opaque_map.Key.t), (data : int)])
  in
  test [];
  [%expect {| (0 0) |}];
  let one_through_nine = List.init 9 ~f:(fun i -> i + 1) in
  test one_through_nine;
  [%expect
    {|
    (0 0)
    (100 1)
    (200 2)
    (300 3)
    (400 4)
    (500 5)
    (600 6)
    (700 7)
    (800 8)
    (900 9)
    |}];
  test (List.map one_through_nine ~f:Int.neg);
  [%expect
    {|
    (-900 -9)
    (-800 -8)
    (-700 -7)
    (-600 -6)
    (-500 -5)
    (-400 -4)
    (-300 -3)
    (-200 -2)
    (-100 -1)
    (0 0)
    |}];
  test (one_through_nine @ List.map one_through_nine ~f:Int.neg);
  [%expect
    {|
    (-900 -9)
    (-800 -8)
    (-700 -7)
    (-600 -6)
    (-500 -5)
    (-400 -4)
    (-300 -3)
    (-200 -2)
    (-100 -1)
    (0 0)
    (100 1)
    (200 2)
    (300 3)
    (400 4)
    (500 5)
    (600 6)
    (700 7)
    (800 8)
    (900 9)
    |}]
;;
