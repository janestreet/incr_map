open Core_kernel
open Import

let subrange map range =
  match range with
  | None -> Map.Using_comparator.empty ~comparator:(Map.comparator map)
  | Some (lower_bound, upper_bound) -> Map.subrange map ~lower_bound ~upper_bound
;;

let setup_subrange map_incr range_incr =
  let slow_submap_incr =
    let open Incr.Let_syntax in
    let%map map = map_incr
    and range = range_incr in
    subrange map range
  in
  let slow_submap = Incr.observe slow_submap_incr in
  let submap = Incr.observe (Incr.Map.subrange map_incr range_incr) in
  slow_submap, submap
;;

let%expect_test "check subrange" =
  let input_map = Incr.Var.create Int.Map.empty in
  let input_range = Incr.Var.create None in
  let map_incr = Incr.Var.watch input_map in
  let range_incr = Incr.Var.watch input_range in
  let slow_submap, submap = setup_subrange map_incr range_incr in
  let print_stable () =
    Incr.stabilize ();
    let result = Incr.Observer.value_exn submap in
    print_s [%sexp (result : string Int.Map.t)];
    [%test_result: string Int.Map.t] ~expect:(Incr.Observer.value_exn slow_submap) result
  in
  print_stable ();
  [%expect {| () |}];
  Incr.Var.set
    input_map
    (Int.Map.of_alist_exn [ 1, "one"; 2, "two"; 3, "three"; 4, "four"; 5, "five" ]);
  print_stable ();
  [%expect {| () |}];
  Incr.Var.set input_range (Some (Incl 1, Incl 3));
  print_stable ();
  [%expect {|
             ((1 one)
              (2 two)
              (3 three)) |}];
  Incr.Var.set input_range (Some (Incl 0, Incl 2));
  Incr.Var.set input_map (Map.set ~key:0 ~data:"zero" (Incr.Var.value input_map));
  print_stable ();
  [%expect {|
             ((0 zero)
              (1 one)
              (2 two)) |}];
  Incr.Var.set input_range (Some (Incl 4, Incl 17));
  print_stable ();
  [%expect {|
             ((4 four)
              (5 five)) |}];
  Incr.Var.set input_range None;
  print_stable ();
  [%expect {| () |}];
  Incr.Var.set input_range (Some (Incl 4, Incl 5));
  print_stable ();
  [%expect {|
             ((4 four)
              (5 five)) |}];
  Incr.Var.set input_range (Some (Incl 5, Incl 4));
  print_stable ();
  [%expect {| () |}];
  (* This test case (lots of changes outside range) should trigger
     the "shortcutting" code path. *)
  Incr.Var.set input_range (Some (Incl 4, Incl 5));
  Incr.stabilize ();
  Incr.Var.set
    input_map
    (Int.Map.of_alist_exn [ 2, "a"; 3, "b"; 4, "c"; 5, "d"; 6, "e"; 7, "f" ]);
  print_stable ();
  [%expect {|
             ((4 c)
              (5 d)) |}]
;;

module Maybe_bound = struct
  include Maybe_bound

  type 'a t = 'a Maybe_bound.t =
    | Incl of 'a
    | Excl of 'a
    | Unbounded
  [@@deriving quickcheck, sexp_of]
end

module Test_operation = struct
  type t =
    | Set_min of int Maybe_bound.t
    | Set_max of int Maybe_bound.t
    | Add of
        { key : int
        ; data : int
        }
    | Remove of float
  [@@deriving sexp_of, quickcheck]

  let quickcheck_generator : t Quickcheck.Generator.t =
    let open Quickcheck.Generator.Let_syntax in
    Quickcheck.Generator.weighted_union
      [ ( 1.0
        , let%map k = Maybe_bound.quickcheck_generator Int.quickcheck_generator in
          Set_min k )
      ; ( 1.0
        , let%map k = Maybe_bound.quickcheck_generator Int.quickcheck_generator in
          Set_max k )
      ; ( 10.0
        , let%map key = Int.quickcheck_generator
          and data = Int.quickcheck_generator in
          Add { key; data } )
      ; ( 10.0
        , let%map i = Float.gen_uniform_excl 0. 1. in
          Remove i )
      ]
  ;;
end

module Initial_state : sig
  (* Not included directly as [Int.Map] so that [@@deriving quickcheck] will work correctly. *)
  type t = int Int.Map.t [@@deriving sexp_of]

  include Quickcheckable.S with type t := t
end = struct
  type t = int Int.Map.t [@@deriving quickcheck, sexp_of]

  let quickcheck_shrinker = quickcheck_shrinker Int.quickcheck_shrinker
  let quickcheck_observer = quickcheck_observer Int.quickcheck_observer

  let quickcheck_generator : t Quickcheck.Generator.t =
    Quickcheck.Generator.list
      (Quickcheck.Generator.tuple2 Int.quickcheck_generator Int.quickcheck_generator)
    |> Quickcheck.Generator.map ~f:(fun l ->
      Int.Map.of_alist_reduce l ~f:(fun _a b -> b))
  ;;
end

module Range = struct
  type t = int Maybe_bound.t * int Maybe_bound.t [@@deriving quickcheck, sexp_of]

  let quickcheck_generator : t Quickcheck.Generator.t =
    let open Quickcheck.Generator.Let_syntax in
    let%map a = [%quickcheck.generator: Int.t Maybe_bound.t]
    and b = [%quickcheck.generator: Int.t Maybe_bound.t] in
    if Maybe_bound.bounds_crossed ~lower:b ~upper:a ~compare:Int.compare
    then a, b
    else b, a
  ;;
end

let apply_op (map, (min, max)) : Test_operation.t -> _ = function
  | Set_min min' ->
    ( map
    , if Maybe_bound.As_lower_bound.compare Int.compare min' max <= 0
      then min', max
      else max, min' )
  | Set_max max' ->
    ( map
    , if Maybe_bound.As_upper_bound.compare Int.compare max' min >= 0
      then min, max'
      else max', min )
  | Add { key; data } -> Map.set map ~key ~data, (min, max)
  | Remove idx ->
    let nth = int_of_float (float_of_int (Map.length map) *. idx) in
    (match Map.nth map nth with
     | None -> map, (min, max)
     | Some (key, _) -> Map.remove map key, (min, max))
;;

module Test_case = struct
  type t =
    { operations : Test_operation.t list
    ; initial_state : Initial_state.t
    ; range : Range.t
    }
  [@@deriving quickcheck, sexp_of]
end

let%expect_test "quickcheck ops test" =
  Expect_test_helpers_kernel.quickcheck
    [%here]
    ~shrinker:Test_case.quickcheck_shrinker
    ~sexp_of:Test_case.sexp_of_t
    Test_case.quickcheck_generator
    ~f:(fun { operations; initial_state; range } ->
      let data = Incr.Var.create (initial_state, range) in
      let incr = Incr.Var.watch data in
      let map_incr = Incr.map incr ~f:fst in
      let range_incr = Incr.map incr ~f:snd |> Incr.map ~f:Option.some in
      let expect_submap, submap = setup_subrange map_incr range_incr in
      let check () =
        Incr.stabilize ();
        [%test_result: int Int.Map.t]
          ~expect:(Incr.Observer.value_exn expect_submap)
          (Incr.Observer.value_exn submap)
      in
      check ();
      List.iter operations ~f:(fun op ->
        Incr.Var.set data (apply_op (Incr.Var.value data) op);
        check ()));
  [%expect {| |}]
;;

let observe incr =
  let observer = Incr.observe incr in
  Incr.stabilize ();
  Incr.Observer.value_exn observer
;;

let subrange_by_rank_test
      ?(initial_map =
        String.Map.of_alist_exn [ "b", (); "d", (); "f", (); "h", (); "j", (); "l", () ])
      ~initial_range
      ~ops
      ()
  =
  (* test subrange_by_rank by starting from [initial_map] and [initial_range] and applying
     ops, which can change both map and range *)
  let map_var = Incr.Var.create initial_map in
  let map = Incr.Var.watch map_var in
  let range_var = Incr.Var.create initial_range in
  let range = Incr.Var.watch range_var in
  let i = Incr.Map.subrange_by_rank map range in
  printf
    !"Initial full            : %{sexp:unit String.Map.t}\n\
      Initial range (%3d, %3d): %{sexp:unit String.Map.t}\n"
    initial_map
    (fst initial_range)
    (snd initial_range)
    (observe i);
  List.iter ops ~f:(fun op ->
    let old_map = Incr.Var.value map_var in
    let old_range = Incr.Var.value range_var in
    let new_map, new_range = op (old_map, old_range) in
    Incr.Var.set map_var new_map;
    Incr.Var.set range_var new_range;
    printf
      !"   Next full            : %{sexp:unit String.Map.t}\n\
       \   Next range (%3d, %3d): %{sexp:unit String.Map.t}\n"
      new_map
      (fst new_range)
      (snd new_range)
      (observe i))
;;

let%expect_test "subrange_by_rank" =
  let map_only_op f (map, range) = f map, range in
  let add key = map_only_op (fun map -> Map.add_exn map ~key ~data:()) in
  let remove key = map_only_op (fun map -> Map.remove map key) in
  let set_range range (map, _range) = map, range in
  subrange_by_rank_test ~initial_range:(1, 3) ~ops:[ Fn.id ] ();
  [%expect
    {|
    Initial full            : ((b ()) (d ()) (f ()) (h ()) (j ()) (l ()))
    Initial range (  1,   3): ((d ()) (f ()) (h ()))
       Next full            : ((b ()) (d ()) (f ()) (h ()) (j ()) (l ()))
       Next range (  1,   3): ((d ()) (f ()) (h ())) |}];
  subrange_by_rank_test ~initial_range:(1, 3) ~ops:[ remove "l" ] ();
  [%expect
    {|
    Initial full            : ((b ()) (d ()) (f ()) (h ()) (j ()) (l ()))
    Initial range (  1,   3): ((d ()) (f ()) (h ()))
       Next full            : ((b ()) (d ()) (f ()) (h ()) (j ()))
       Next range (  1,   3): ((d ()) (f ()) (h ())) |}];
  subrange_by_rank_test ~initial_range:(1, 3) ~ops:[ add "a" ] ();
  [%expect
    {|
    Initial full            : ((b ()) (d ()) (f ()) (h ()) (j ()) (l ()))
    Initial range (  1,   3): ((d ()) (f ()) (h ()))
       Next full            : ((a ()) (b ()) (d ()) (f ()) (h ()) (j ()) (l ()))
       Next range (  1,   3): ((b ()) (d ()) (f ())) |}];
  subrange_by_rank_test ~initial_range:(1, 3) ~ops:[ add "g" ] ();
  [%expect
    {|
    Initial full            : ((b ()) (d ()) (f ()) (h ()) (j ()) (l ()))
    Initial range (  1,   3): ((d ()) (f ()) (h ()))
       Next full            : ((b ()) (d ()) (f ()) (g ()) (h ()) (j ()) (l ()))
       Next range (  1,   3): ((d ()) (f ()) (g ())) |}];
  subrange_by_rank_test ~initial_range:(1, 3) ~ops:[ remove "b" ] ();
  [%expect
    {|
    Initial full            : ((b ()) (d ()) (f ()) (h ()) (j ()) (l ()))
    Initial range (  1,   3): ((d ()) (f ()) (h ()))
       Next full            : ((d ()) (f ()) (h ()) (j ()) (l ()))
       Next range (  1,   3): ((f ()) (h ()) (j ())) |}];
  subrange_by_rank_test ~initial_range:(1, 3) ~ops:[ remove "d" ] ();
  [%expect
    {|
    Initial full            : ((b ()) (d ()) (f ()) (h ()) (j ()) (l ()))
    Initial range (  1,   3): ((d ()) (f ()) (h ()))
       Next full            : ((b ()) (f ()) (h ()) (j ()) (l ()))
       Next range (  1,   3): ((f ()) (h ()) (j ())) |}];
  subrange_by_rank_test ~initial_range:(1, 3) ~ops:[ remove "f"; add "g" ] ();
  [%expect
    {|
    Initial full            : ((b ()) (d ()) (f ()) (h ()) (j ()) (l ()))
    Initial range (  1,   3): ((d ()) (f ()) (h ()))
       Next full            : ((b ()) (d ()) (h ()) (j ()) (l ()))
       Next range (  1,   3): ((d ()) (h ()) (j ()))
       Next full            : ((b ()) (d ()) (g ()) (h ()) (j ()) (l ()))
       Next range (  1,   3): ((d ()) (g ()) (h ()))
     |}];
  subrange_by_rank_test ~initial_range:(0, 0) ~ops:[ add "a" ] ();
  [%expect
    {|
    Initial full            : ((b ()) (d ()) (f ()) (h ()) (j ()) (l ()))
    Initial range (  0,   0): ((b ()))
       Next full            : ((a ()) (b ()) (d ()) (f ()) (h ()) (j ()) (l ()))
       Next range (  0,   0): ((a ())) |}];
  subrange_by_rank_test ~initial_range:(5, 100) ~ops:[ add "m" ] ();
  [%expect
    {|
    Initial full            : ((b ()) (d ()) (f ()) (h ()) (j ()) (l ()))
    Initial range (  5, 100): ((l ()))
       Next full            : ((b ()) (d ()) (f ()) (h ()) (j ()) (l ()) (m ()))
       Next range (  5, 100): ((l ()) (m ())) |}];
  subrange_by_rank_test ~initial_range:(5, 100) ~ops:[ add "a" ] ();
  [%expect
    {|
    Initial full            : ((b ()) (d ()) (f ()) (h ()) (j ()) (l ()))
    Initial range (  5, 100): ((l ()))
       Next full            : ((a ()) (b ()) (d ()) (f ()) (h ()) (j ()) (l ()))
       Next range (  5, 100): ((j ()) (l ())) |}];
  subrange_by_rank_test ~initial_range:(6, 6) ~ops:[ add "m" ] ();
  [%expect
    {|
    Initial full            : ((b ()) (d ()) (f ()) (h ()) (j ()) (l ()))
    Initial range (  6,   6): ()
       Next full            : ((b ()) (d ()) (f ()) (h ()) (j ()) (l ()) (m ()))
       Next range (  6,   6): ((m ())) |}];
  subrange_by_rank_test
    ~initial_range:(1, 2)
    ~ops:[ (fun m -> add "c" (remove "d" m)) ]
    ();
  [%expect
    {|
    Initial full            : ((b ()) (d ()) (f ()) (h ()) (j ()) (l ()))
    Initial range (  1,   2): ((d ()) (f ()))
       Next full            : ((b ()) (c ()) (f ()) (h ()) (j ()) (l ()))
       Next range (  1,   2): ((c ()) (f ()))|}];
  subrange_by_rank_test
    ~initial_range:(1, 2)
    ~ops:[ (fun m -> add "e" (remove "d" m)) ]
    ();
  [%expect
    {|
    Initial full            : ((b ()) (d ()) (f ()) (h ()) (j ()) (l ()))
    Initial range (  1,   2): ((d ()) (f ()))
       Next full            : ((b ()) (e ()) (f ()) (h ()) (j ()) (l ()))
       Next range (  1,   2): ((e ()) (f ()))|}];
  subrange_by_rank_test
    ~initial_range:(1, 2)
    ~ops:[ (fun m -> remove "d" m |> remove "b") ]
    ();
  [%expect
    {|
    Initial full            : ((b ()) (d ()) (f ()) (h ()) (j ()) (l ()))
    Initial range (  1,   2): ((d ()) (f ()))
       Next full            : ((f ()) (h ()) (j ()) (l ()))
       Next range (  1,   2): ((h ()) (j ()))|}];
  subrange_by_rank_test
    ~initial_range:(1, 2)
    ~ops:[ (fun m -> add "e" (remove "f" m)) ]
    ();
  [%expect
    {|
    Initial full            : ((b ()) (d ()) (f ()) (h ()) (j ()) (l ()))
    Initial range (  1,   2): ((d ()) (f ()))
       Next full            : ((b ()) (d ()) (e ()) (h ()) (j ()) (l ()))
       Next range (  1,   2): ((d ()) (e ()))|}];
  subrange_by_rank_test
    ~initial_range:(1, 2)
    ~ops:[ (fun m -> add "g" (remove "f" m)) ]
    ();
  [%expect
    {|
    Initial full            : ((b ()) (d ()) (f ()) (h ()) (j ()) (l ()))
    Initial range (  1,   2): ((d ()) (f ()))
       Next full            : ((b ()) (d ()) (g ()) (h ()) (j ()) (l ()))
       Next range (  1,   2): ((d ()) (g ()))|}];
  subrange_by_rank_test
    ~initial_range:(1, 3)
    ~ops:
      [ map_only_op (fun _ ->
          String.Map.of_alist_exn
            [ "a", (); "c", (); "e", (); "g", (); "i", (); "k", () ])
      ]
    ();
  [%expect
    {|
    Initial full            : ((b ()) (d ()) (f ()) (h ()) (j ()) (l ()))
    Initial range (  1,   3): ((d ()) (f ()) (h ()))
       Next full            : ((a ()) (c ()) (e ()) (g ()) (i ()) (k ()))
       Next range (  1,   3): ((c ()) (e ()) (g ()))|}];
  subrange_by_rank_test ~initial_range:(1, 2) ~ops:[ set_range (1, 3) ] ();
  [%expect
    {|
     Initial full            : ((b ()) (d ()) (f ()) (h ()) (j ()) (l ()))
     Initial range (  1,   2): ((d ()) (f ()))
        Next full            : ((b ()) (d ()) (f ()) (h ()) (j ()) (l ()))
        Next range (  1,   3): ((d ()) (f ()) (h ()))|}];
  subrange_by_rank_test ~initial_range:(50, 60) ~ops:[ set_range (1, 60) ] ();
  [%expect
    {|
     Initial full            : ((b ()) (d ()) (f ()) (h ()) (j ()) (l ()))
     Initial range ( 50,  60): ()
        Next full            : ((b ()) (d ()) (f ()) (h ()) (j ()) (l ()))
        Next range (  1,  60): ((d ()) (f ()) (h ()) (j ()) (l ()))|}];
  subrange_by_rank_test
    ~initial_range:(1, 2)
    ~ops:[ (fun x -> add "a" x |> set_range (2, 3)) ]
    ();
  [%expect
    {|
     Initial full            : ((b ()) (d ()) (f ()) (h ()) (j ()) (l ()))
     Initial range (  1,   2): ((d ()) (f ()))
        Next full            : ((a ()) (b ()) (d ()) (f ()) (h ()) (j ()) (l ()))
        Next range (  2,   3): ((d ()) (f ()))|}];
  subrange_by_rank_test
    ~initial_range:(1, 2)
    ~ops:[ set_range (2, 3); set_range (3, 3); set_range (3, 4) ]
    ();
  [%expect
    {|
     Initial full            : ((b ()) (d ()) (f ()) (h ()) (j ()) (l ()))
     Initial range (  1,   2): ((d ()) (f ()))
        Next full            : ((b ()) (d ()) (f ()) (h ()) (j ()) (l ()))
        Next range (  2,   3): ((f ()) (h ()))
        Next full            : ((b ()) (d ()) (f ()) (h ()) (j ()) (l ()))
        Next range (  3,   3): ((h ()))
        Next full            : ((b ()) (d ()) (f ()) (h ()) (j ()) (l ()))
        Next range (  3,   4): ((h ()) (j ()))|}];
  subrange_by_rank_test
    ~initial_range:(1, 2)
    ~ops:[ (fun x -> remove "f" x |> set_range (2, 3)) ]
    ();
  [%expect
    {|
     Initial full            : ((b ()) (d ()) (f ()) (h ()) (j ()) (l ()))
     Initial range (  1,   2): ((d ()) (f ()))
        Next full            : ((b ()) (d ()) (h ()) (j ()) (l ()))
        Next range (  2,   3): ((h ()) (j ()))|}];
  subrange_by_rank_test
    ~initial_range:(1, 5)
    ~ops:[ (fun x -> remove "l" x |> set_range (1, 4)) ]
    ();
  [%expect
    {|
     Initial full            : ((b ()) (d ()) (f ()) (h ()) (j ()) (l ()))
     Initial range (  1,   5): ((d ()) (f ()) (h ()) (j ()) (l ()))
        Next full            : ((b ()) (d ()) (f ()) (h ()) (j ()))
        Next range (  1,   4): ((d ()) (f ()) (h ()) (j ()))|}]
;;

(* Naively collect elements between [from] and [to_], inclusive.

   This is purposedly not using Map.nth + Incr_map.subrange to be more different from
   the Incr_map.subrange_by_rank implementation to avoid same bugs in both.

*)
let subrange_by_rank_reference m (from, to_) =
  let comparator = Map.comparator_s m in
  let seq = Map.to_sequence ~order:`Increasing_key m in
  let seq = Sequence.drop seq from in
  let seq = Sequence.take seq (to_ - from + 1) in
  Map.of_increasing_sequence comparator seq |> Or_error.ok_exn
;;

open Subrange_quickcheck_helper

let%test_unit "quickcheck subrange_by_rank" =
  Quickcheck.test
    ~sexp_of:[%sexp_of: (map_op * range_op) list * map * (int * int)]
    (Quickcheck.Generator.tuple3
       (Quickcheck.Generator.list (map_and_range_op_gen ()))
       map_gen
       range_gen)
    ~f:(fun (ops, map, range) ->
      let map_var = Incr.Var.create map in
      let range_var = Incr.Var.create range in
      let submap =
        Incr.Map.subrange_by_rank (Incr.Var.watch map_var) (Incr.Var.watch range_var)
      in
      let check () =
        [%test_result: map]
          ~expect:
            (subrange_by_rank_reference
               (Incr.Var.value map_var)
               (Incr.Var.value range_var))
          (observe submap)
      in
      check ();
      List.iter ops ~f:(fun (map_op, range_op) ->
        apply_map_op_incr map_var map_op;
        apply_range_op_incr range_var range_op;
        check ()))
;;

let%test_unit "quickcheck subrange_by_rank large fixed range" =
  let open Quickcheck in
  let map_small_op_gen = map_op_gen ~key_range:(0, 300) () in
  let map_gen =
    let%bind_open.Generator len = Generator.of_list [ 300; 10000 ] in
    map_with_length_gen ~key_range:(-10, 20000) len
  in
  Quickcheck.test
    ~trials:2
    (Quickcheck.Generator.tuple2
       (Quickcheck.Generator.list_with_length
          100
          (Quickcheck.Generator.list_with_length 30 map_small_op_gen))
       map_gen)
    ~f:(fun (updates, map) ->
      let range_begin, range_end = 10, 100 in
      let range = range_begin, range_end in
      let var = Incr.Var.create map in
      let submap = Incr.Map.subrange_by_rank (Incr.Var.watch var) (Incr.return range) in
      let check () =
        let got = observe submap in
        [%test_result: map]
          ~expect:(subrange_by_rank_reference (Incr.Var.value var) range)
          got;
        [%test_result: int] ~expect:(range_end - range_begin + 1) (Map.length got)
      in
      check ();
      List.iter updates ~f:(fun ops ->
        let map =
          List.fold ops ~init:(Incr.Var.value var) ~f:(fun acc op ->
            apply_map_op acc op)
        in
        Incr.Var.set var map;
        check ()))
;;
