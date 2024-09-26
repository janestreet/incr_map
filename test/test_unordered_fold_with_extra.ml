open! Core
open! Import

let unordered_fold_with_extra
  (type k v a)
  ~(data_equal : v -> v -> bool)
  ~(acc_equal : a -> a -> bool)
  ?specialized_initial
  (m : (k, v, _) Map.t Incr.t)
  ~extra
  ~(init : a)
  ~add
  ~remove
  ~extra_changed
  =
  let a =
    Incr.Map.unordered_fold_with_extra
      ~data_equal
      ?specialized_initial
      m
      extra
      ~init
      ~add
      ~remove
      ~extra_changed
  in
  let b =
    let%map m and extra in
    Map.fold m ~init ~f:(fun ~key ~data acc -> add ~key ~data acc extra)
  in
  let%map a and b in
  require (acc_equal a b);
  a
;;

let%expect_test _ =
  let map = Incr.Var.create (String.Map.of_alist_exn [ "a", 1; "b", 2 ]) in
  let extra = Incr.Var.create 2 in
  let sum_o =
    unordered_fold_with_extra
      (Incr.Var.watch map)
      ~extra:(Incr.Var.watch extra)
      ~data_equal:Int.equal
      ~acc_equal:Int.equal
      ~init:0
      ~add:(fun ~key:_ ~data:v acc extra -> acc + (v * extra))
      ~remove:(fun ~key:_ ~data:v acc extra -> acc - (v * extra))
      ~extra_changed:(fun ~old_extra ~new_extra ~input:_ acc ->
        acc / old_extra * new_extra)
    |> Incr.observe
  in
  let dump () =
    Incr.stabilize ();
    let value = Incr.Observer.value_exn in
    print_s ([%sexp_of: int] (value sum_o))
  in
  let change f = Incr.Var.set map (f (Incr.Var.value map)) in
  let change_extra v = Incr.Var.set extra v in
  dump ();
  [%expect {| 6 |}];
  change (fun m -> Map.set m ~key:"c" ~data:4);
  dump ();
  [%expect {| 14 |}];
  change_extra 5;
  dump ();
  [%expect {| 35 |}];
  change (fun m -> Map.set m ~key:"c" ~data:0);
  change_extra 3;
  dump ();
  [%expect {| 9 |}]
;;

let%expect_test "test specialized_initial" =
  let map = Incr.Var.create (String.Map.of_alist_exn [ "a", 1; "b", 2 ]) in
  let map_o = Incr.observe (Incr.Var.watch map) in
  let extra = Incr.Var.create 2 in
  let specialized_initial_is_called = ref false in
  let sum_o =
    unordered_fold_with_extra
      (Incr.Var.watch map)
      ~extra:(Incr.Var.watch extra)
      ~data_equal:Int.equal
      ~acc_equal:Int.equal
      ~init:0
      ~add:(fun ~key:_ ~data:v acc extra -> acc + (v * extra))
      ~remove:(fun ~key:_ -> assert false)
      ~extra_changed:(fun ~old_extra ~new_extra ~input:_ acc ->
        acc / old_extra * new_extra)
      ~specialized_initial:(fun ~init:_ map extra ->
        specialized_initial_is_called := true;
        extra * Core.List.sum (module Int) ~f:Fn.id (Map.data map))
    |> Incr.observe
  in
  let dump () =
    Incr.stabilize ();
    let value = Incr.Observer.value_exn in
    print_s ([%sexp_of: int * int String.Map.t] (value sum_o, value map_o))
  in
  dump ();
  assert !specialized_initial_is_called;
  [%expect
    {|
    (6 (
      (a 1)
      (b 2)))
    |}]
;;

let%expect_test "filter-reuse" =
  let map =
    Incr.Var.create (String.Map.of_alist_exn [ "aa", 1; "a", 2; "b", 3; "ab", 4 ])
  in
  let extra = Incr.Var.create "" in
  let add ~key ~data acc substring =
    if String.is_substring key ~substring then Map.set acc ~key ~data else acc
  in
  let remove ~key ~data:_ acc _extra = Map.remove acc key in
  let extra_changed ~old_extra ~new_extra ~input acc =
    let to_filter =
      if String.is_substring new_extra ~substring:old_extra then acc else input
    in
    Map.filter_keys to_filter ~f:(String.is_substring ~substring:new_extra)
  in
  let sum_o =
    unordered_fold_with_extra
      (Incr.Var.watch map)
      ~extra:(Incr.Var.watch extra)
      ~data_equal:Int.equal
      ~acc_equal:[%equal: int String.Map.t]
      ~init:String.Map.empty
      ~add
      ~remove
      ~extra_changed
    |> Incr.observe
  in
  let dump () =
    Incr.stabilize ();
    let value = Incr.Observer.value_exn in
    print_s ([%sexp_of: string * int String.Map.t] (Incr.Var.value extra, value sum_o))
  in
  dump ();
  [%expect
    {|
    ("" (
      (a  2)
      (aa 1)
      (ab 4)
      (b  3)))
    |}];
  Incr.Var.set extra "a";
  dump ();
  [%expect
    {|
    (a (
      (a  2)
      (aa 1)
      (ab 4)))
    |}];
  Incr.Var.set extra "aa";
  dump ();
  [%expect {| (aa ((aa 1))) |}];
  Incr.Var.set extra "b";
  dump ();
  [%expect
    {|
    (b (
      (ab 4)
      (b  3)))
    |}]
;;
