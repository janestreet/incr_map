open! Core
open! Import

module Collapsed = struct
  module T = struct
    type t = (int, int) Tuple2.t [@@deriving sexp_of]

    type comparator_witness =
      (Int.comparator_witness, Int.comparator_witness) Tuple2.comparator_witness

    let comparator = Tuple2.comparator Int.comparator Int.comparator
  end

  include T
  include Comparable.Make_plain_using_comparator (T)
end

let%expect_test "manual updates" =
  let var =
    [ 0, [ 1, "a" ]; 1, [ 2, "b" ] ]
    |> List.map ~f:(fun (outer_key, inner_alist) ->
      outer_key, Int.Map.of_alist_exn inner_alist)
    |> Int.Map.of_alist_exn
    |> Incr.Var.create
  in
  let observer =
    Incr.observe (Incr_map.collapse ~comparator:(module Int) (Incr.Var.watch var))
  in
  let update_and_test ~f =
    Incr.Var.replace var ~f;
    Incr.stabilize ();
    print_s [%sexp (Incr.Observer.value_exn observer : string Collapsed.Map.t)]
  in
  update_and_test ~f:Fn.id;
  [%expect
    {|
    (((0 1) a)
     ((1 2) b))
    |}];
  update_and_test ~f:(fun m -> Map.add_exn m ~key:2 ~data:(Int.Map.singleton 4 "c"));
  [%expect
    {|
    (((0 1) a)
     ((1 2) b)
     ((2 4) c))
    |}];
  update_and_test ~f:(fun m -> Map.remove m 1);
  [%expect
    {|
    (((0 1) a)
     ((2 4) c))
    |}];
  update_and_test ~f:(fun m -> Map.set m ~key:2 ~data:(Int.Map.singleton 0 "c"));
  [%expect
    {|
    (((0 1) a)
     ((2 0) c))
    |}];
  update_and_test ~f:(fun m ->
    Map.update m 2 ~f:(function
      | None -> assert false
      | Some m -> Map.add_exn m ~key:1 ~data:"asdf"));
  [%expect
    {|
    (((0 1) a)
     ((2 0) c)
     ((2 1) asdf))
    |}]
;;

let all_at_once t =
  Map.fold
    t
    ~init:
      (Map.Using_comparator.empty
         ~comparator:(Tuple2.comparator Int.comparator Int.comparator))
    ~f:(fun ~key:outer_key ~data acc ->
      Map.fold data ~init:acc ~f:(fun ~key:inner_key ~data acc ->
        Map.add_exn acc ~key:(outer_key, inner_key) ~data))
;;

let%test_unit "randomized map changes" =
  let var = Incr.Var.create Int.Map.empty in
  let observer =
    Incremental.observe (Incr_map.collapse (Incr.Var.watch var) ~comparator:(module Int))
  in
  Quickcheck.test
    (Map_operations.nested_quickcheck_generator String.quickcheck_generator)
    ~f:(fun operations ->
      Map_operations.nested_run_operations
        operations
        ~inner_map_comparator:(module Int)
        ~into:var
        ~after_stabilize:(fun () ->
          [%test_result: string Collapsed.Map.t]
            ~expect:(all_at_once (Incr.Var.latest_value var))
            (Incremental.Observer.value_exn observer)))
;;

let%test_unit "collapse expand compose" =
  let var = Incr.Var.create Int.Map.empty in
  let observer =
    Incremental.observe
      (Incr_map.expand
         (Incr_map.collapse ~comparator:(module Int) (Incr.Var.watch var))
         ~outer_comparator:(module Int)
         ~inner_comparator:(module Int))
  in
  Quickcheck.test
    (Map_operations.nested_quickcheck_generator String.quickcheck_generator)
    ~f:(fun operations ->
      Map_operations.nested_run_operations
        operations
        ~inner_map_comparator:(module Int)
        ~into:var
        ~after_stabilize:(fun () ->
          [%test_result: string Int.Map.t Int.Map.t]
          (* NB: outer keys that map to an empty inner map will be dropped by this
             operation. *)
            ~expect:(Map.filter (Incr.Var.latest_value var) ~f:(Fn.non Map.is_empty))
            (Incremental.Observer.value_exn observer)))
;;

module%test [@name "[collapse_by] and [collapse_by_loosened_requirements] manual tests"] _ =
struct
  let test ~version first_outer_key second_outer_key =
    let module I = Incremental.Make () in
    let var = I.Var.create Int.Map.empty in
    (* Given a nested map, this Incremental node should return a flattened map using the
       nested keys. This should be valid as long as the nested keys are unique. *)
    let operation =
      match version with
      | `Require_bijectivity -> Incr_map.collapse_by
      | `Don't_require_bijectivity -> Incr_map.collapse_by_loosened_requirements
    in
    let obs =
      I.Var.watch var
      |> operation ~comparator:(module Int) ~merge_keys:(fun _ nested_key -> nested_key)
      |> I.observe
    in
    let set_stabilize outer_key =
      (* There is only one nested key, so this should always work *)
      I.Var.set var (Int.Map.singleton outer_key (Int.Map.singleton 0 0));
      I.stabilize ();
      I.Observer.value_exn obs
    in
    let (_ : int Int.Map.t) = set_stabilize first_outer_key in
    let (_ : int Int.Map.t) = set_stabilize second_outer_key in
    ()
  ;;

  let print_test ~version first_outer_key second_outer_key =
    try
      test ~version first_outer_key second_outer_key;
      print_endline [%string "Passed %{first_outer_key#Int} -> %{second_outer_key#Int}"]
    with
    | exn ->
      print_endline
        [%string "Failed %{first_outer_key#Int} -> %{second_outer_key#Int}: %{exn#Exn}"]
  ;;

  let%expect_test "require bijectivity" =
    let print_test = print_test ~version:`Require_bijectivity in
    (* Increasing outer-keys works *)
    print_test 0 1;
    [%expect {| Passed 0 -> 1 |}];
    (* Decreasing outer-keys fail *)
    print_test 1 0;
    [%expect {| Failed 1 -> 0: ("[Map.add_exn] got key already present" (key 0)) |}];
    (* More proof this is comparison-based *)
    let first_outer_key = 3 in
    List.iter (List.range 0 10) ~f:(fun second_outer_key ->
      print_test first_outer_key second_outer_key);
    [%expect
      {|
      Failed 3 -> 0: ("[Map.add_exn] got key already present" (key 0))
      Failed 3 -> 1: ("[Map.add_exn] got key already present" (key 0))
      Failed 3 -> 2: ("[Map.add_exn] got key already present" (key 0))
      Passed 3 -> 3
      Passed 3 -> 4
      Passed 3 -> 5
      Passed 3 -> 6
      Passed 3 -> 7
      Passed 3 -> 8
      Passed 3 -> 9
      |}]
  ;;

  let%expect_test "don't require bijectivity" =
    let print_test = print_test ~version:`Don't_require_bijectivity in
    (* Increasing outer-keys works *)
    print_test 0 1;
    [%expect {| Passed 0 -> 1 |}];
    (* Decreasing outer-keys fail *)
    print_test 1 0;
    [%expect {| Passed 1 -> 0 |}];
    (* More proof this is comparison-based *)
    let first_outer_key = 3 in
    List.iter (List.range 0 10) ~f:(fun second_outer_key ->
      print_test first_outer_key second_outer_key);
    [%expect
      {|
      Passed 3 -> 0
      Passed 3 -> 1
      Passed 3 -> 2
      Passed 3 -> 3
      Passed 3 -> 4
      Passed 3 -> 5
      Passed 3 -> 6
      Passed 3 -> 7
      Passed 3 -> 8
      Passed 3 -> 9
      |}]
  ;;
end

module%test
  [@name "[collapse_by] and [collapse_by_loosened_requirements] quickcheck tests"] _ =
struct
  module Two_ints : sig
    type t [@@deriving compare, sexp_of]

    include Comparator.S with type t := t

    val create : int -> int -> t
  end = struct
    module T = struct
      type t = int * int [@@deriving compare, sexp_of]
    end

    include T

    (* This is a weird but legal way to build values of this type. The onus would be on
       the caller to make sure that there are no unordered tuples that overlap as keys,
       but if they can do that, then [Incr_map.collapse_by] should be fine. *)
    let create a b = Int.min a b, Int.max a b

    include Comparator.Make (T)
  end

  module Small_int = struct
    include Int

    let quickcheck_generator = Int.gen_incl 0 20
  end

  let all_merged_keys outer_map =
    let%bind.List a, inner_map = Map.to_alist outer_map in
    let%bind.List b = Map.keys inner_map in
    List.return (Two_ints.create a b)
  ;;

  let potentially_invalid_nested_map_generator =
    [%quickcheck.generator: unit Map.M(Small_int).t Map.M(Small_int).t]
  ;;

  let valid_nested_map_generator =
    Quickcheck.Generator.filter
      potentially_invalid_nested_map_generator
      ~f:(fun outer_map ->
        (* this filter on the quickcheck-generated maps is for making sure that no two
           combined keys are the same. *)
        let all_merged_keys = all_merged_keys outer_map in
        not (List.contains_dup all_merged_keys ~compare:[%compare: Two_ints.t]))
  ;;

  (* the keys are generated by quickcheck, so don't include them in expect test output *)
  let censor_key ~error_string =
    let error = Sexp.of_string error_string in
    let rec loop : Sexp.t -> Sexp.t = function
      | List [ Atom "key"; _ ] -> List [ Atom "key"; Atom "<censored>" ]
      | Atom a -> Atom a
      | List l -> List (List.map ~f:loop l)
    in
    print_s (loop error)
  ;;

  let non_incrementally outer_map =
    all_merged_keys outer_map
    |> List.map ~f:(fun key -> key, ())
    |> Map.of_alist_reduce (module Two_ints) ~f:(fun _ b -> b)
  ;;

  let require_maps_are_equal a b ~data_equal ~sexp_of_key ~sexp_of_data =
    match Sequence.to_list (Map.symmetric_diff a b ~data_equal) with
    | [] -> ()
    | _ :: _ as diffs ->
      print_s
        [%message
          "Maps not equal" (diffs : (key, data) Map.Symmetric_diff_element.t list)]
  ;;

  let run_test ~generator_to_use ~function_to_use =
    let map_generator =
      match generator_to_use with
      | `Always_valid -> valid_nested_map_generator
      | `Not_always_valid -> potentially_invalid_nested_map_generator
    in
    Quickcheck.test
      (Quickcheck.Generator.tuple3 map_generator map_generator map_generator)
      ~f:(fun (first_map, second_map, third_map) ->
        let module I = Incremental.Make () in
        let var = I.Var.create Int.Map.empty in
        let operation =
          match function_to_use with
          | `Require_bijectivity -> Incr_map.collapse_by
          | `Don't_require_bijectivity -> Incr_map.collapse_by_loosened_requirements
        in
        let obs =
          I.Var.watch var
          |> operation ~comparator:(module Two_ints) ~merge_keys:(fun a b ->
            Two_ints.create a b)
          |> I.observe
        in
        let set_stabilize map =
          (* There is only one nested key, so this should always work *)
          I.Var.set var map;
          I.stabilize ();
          let incremental_result = I.Observer.value_exn obs in
          let nonincremental_result = non_incrementally map in
          require_maps_are_equal
            incremental_result
            nonincremental_result
            ~data_equal:[%equal: unit]
            ~sexp_of_key:[%sexp_of: opaque]
            ~sexp_of_data:[%sexp_of: unit]
        in
        set_stabilize first_map;
        set_stabilize second_map;
        set_stabilize third_map)
  ;;

  let%expect_test "fail when run with version that requires bijectivity" =
    Expect_test_helpers_core.require_does_raise ~show_backtrace:false (fun () ->
      run_test ~generator_to_use:`Always_valid ~function_to_use:`Require_bijectivity);
    censor_key ~error_string:[%expect.output];
    [%expect
      {|
      ("Base_quickcheck.Test.run: test failed"
        (input _)
        (error ("[Map.add_exn] got key already present" (key <censored>))))
      |}]
  ;;

  let%expect_test "pass when run with version that doesn't require bijectivity" =
    run_test ~generator_to_use:`Always_valid ~function_to_use:`Don't_require_bijectivity
  ;;

  let%expect_test "failure mode when the user doesn't uphold the invariant" =
    run_test
      ~generator_to_use:`Not_always_valid
      ~function_to_use:`Don't_require_bijectivity;
    [%expect.output]
    |> String.split_lines
    |> String.Set.of_list
    |> Set.iter ~f:print_endline;
    [%expect {| ("Maps not equal" (diffs ((<opaque> (Right ()))))) |}]
  ;;
end
