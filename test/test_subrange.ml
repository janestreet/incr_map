open Core_kernel
open Import

let subrange map range =
  let empty = Map.Using_comparator.empty ~comparator:(Map.comparator map) in
  match range with
  | None -> empty
  | Some (min, max) ->
    Map.fold_range_inclusive map ~min ~max ~init:empty ~f:(fun ~key ~data map ->
      Map.set map ~key ~data
    )

let setup_subrange map_incr range_incr =
  let slow_submap_incr =
    let open Incr.Let_syntax in
    let%map map = map_incr and range = range_incr in
    subrange map range
  in

  let slow_submap = Incr.observe slow_submap_incr in
  let submap = Incr.observe (Incr.Map.subrange map_incr range_incr) in
  (slow_submap, submap)

let%expect_test "check subrange" =
  let input_map = Incr.Var.create Int.Map.empty in
  let input_range = Incr.Var.create None in

  let map_incr = Incr.Var.watch input_map in
  let range_incr = Incr.Var.watch input_range in

  let (slow_submap, submap) = setup_subrange map_incr range_incr in

  let print_stable () =
    Incr.stabilize ();
    let result = Incr.Observer.value_exn submap in
    print_s [%sexp (result : string Int.Map.t)];
    [%test_result: string Int.Map.t]
      ~expect:(Incr.Observer.value_exn slow_submap)
      result
  in

  print_stable ();
  [%expect {| () |}];

  Incr.Var.set input_map
    (Int.Map.of_alist_exn
       [ 1, "one"
       ; 2, "two"
       ; 3, "three"
       ; 4, "four"
       ; 5, "five"
       ]);

  print_stable ();
  [%expect {| () |}];

  Incr.Var.set input_range (Some (1, 3));

  print_stable ();
  [%expect {|
             ((1 one)
              (2 two)
              (3 three)) |}];

  Incr.Var.set input_range (Some (0, 2));
  Incr.Var.set input_map (Map.set ~key:0 ~data:"zero" (Incr.Var.value input_map));

  print_stable ();
  [%expect {|
             ((0 zero)
              (1 one)
              (2 two)) |}];

  Incr.Var.set input_range (Some (4, 17));

  print_stable ();
  [%expect {|
             ((4 four)
              (5 five)) |}];

  Incr.Var.set input_range None;

  print_stable ();
  [%expect {| () |}];

  Incr.Var.set input_range (Some (4, 5));

  print_stable ();
  [%expect {|
             ((4 four)
              (5 five)) |}];

  Incr.Var.set input_range (Some (5, 4));

  print_stable ();
  [%expect {| () |}];

  (* This test case (lots of changes outside range) should trigger
     the "shortcutting" code path. *)
  Incr.Var.set input_range (Some (4, 5));
  Incr.stabilize ();

  Incr.Var.set input_map
    (Int.Map.of_alist_exn
       [ 2, "a"
       ; 3, "b"
       ; 4, "c"
       ; 5, "d"
       ; 6, "e"
       ; 7, "f"
       ]);

  print_stable ();
  [%expect {|
             ((4 c)
              (5 d)) |}];
;;

type gen_op =
  [ `Set_min of int
  | `Set_max of int
  | `Add of (int * int)
  | `Remove of float
  ]

let gen_op : gen_op Quickcheck.Generator.t =
  let open Quickcheck.Generator.Let_syntax in
  Quickcheck.Generator.weighted_union
    [ (  1.0, let%map k = Int.gen in `Set_min k)
    ; (  1.0, let%map k = Int.gen in `Set_max k)
    ; ( 10.0, let%map k = Int.gen and v = Int.gen in `Add (k, v))
    ; ( 10.0, let%map i = Float.gen_uniform_excl 0. 1. in `Remove i)
    ]

let map_gen : int Int.Map.t Quickcheck.Generator.t =
  Quickcheck.Generator.list (Quickcheck.Generator.tuple2 Int.gen Int.gen)
  |> Quickcheck.Generator.map ~f:(fun l ->
    List.fold l ~init:Int.Map.empty ~f:(fun map (key, data) ->
      Map.set map ~key ~data
    )
  )

let range_gen : (int * int) Quickcheck.Generator.t =
  let open Quickcheck.Generator.Let_syntax in
  let%map a = Int.gen and b = Int.gen in
  if a < b then (a, b) else (b, a)

let apply_op (map, (min, max)) = function
  | `Set_min min' -> (map, if min' <= max then (min', max) else (max, min'))
  | `Set_max max' -> (map, if max' >= min then (min, max') else (max', min))
  | `Add (key, data) -> (Map.set map ~key ~data, (min, max))
  | `Remove idx ->
    let nth = int_of_float ((float_of_int (Map.length map)) *. idx) in
    match Map.nth map nth with
    | None -> (map, (min, max))
    | Some (key, _) -> (Map.remove map key, (min, max))

let%test_unit "quickcheck ops test" =
  Quickcheck.test
    (Quickcheck.Generator.tuple3
       (Quickcheck.Generator.list gen_op)
       map_gen
       range_gen
    )
    ~f:(fun (ops, map, range) ->
      let data = Incr.Var.create (map, range) in
      let incr = Incr.Var.watch data in
      let map_incr = Incr.map incr ~f:fst in
      let range_incr = Incr.map incr ~f:snd |> Incr.map ~f:Option.some in
      let (expect_submap, submap) = setup_subrange map_incr range_incr in

      let check () =
        Incr.stabilize ();
        [%test_result: int Int.Map.t]
          ~expect:(Incr.Observer.value_exn expect_submap)
          (Incr.Observer.value_exn submap);
      in

      check ();
      List.iter ops ~f:(fun op ->
        Incr.Var.set data (apply_op (Incr.Var.value data) op);
        check ();
      )
    )

