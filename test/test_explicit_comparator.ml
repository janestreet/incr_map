open Core
open Import

let count_nodes node =
  let count = ref 0 in
  Incremental.For_analyzer.traverse
    [ Incremental.pack node ]
    ~add_node:
      (fun
        ~id:_
        ~kind:_
        ~cutoff:_
        ~children:_
        ~bind_children:_
        ~user_info:_
        ~recomputed_at:_
        ~changed_at:_
        ~height:_
      -> incr count);
  !count
;;

let%expect_test "incremental graph size with and without explicit comparators" =
  let init = Incr.Var.create Int.Map.empty |> Incr.Var.watch in
  let without_explicit_comparator =
    Incr_map.filter_mapi' init ~f:(fun ~key:_ ~data -> Incr.map ~f:Option.return data)
  in
  let with_explicit_comparator =
    Incr_map.filter_mapi'
      init
      ~comparator:(module Int)
      ~f:(fun ~key:_ ~data -> Incr.map ~f:Option.return data)
  in
  let o1 = Incr.observe without_explicit_comparator in
  let o2 = Incr.observe with_explicit_comparator in
  Incr.stabilize ();
  let without_explicit_comparator = count_nodes without_explicit_comparator in
  let with_explicit_comparator = count_nodes with_explicit_comparator in
  print_s [%message (without_explicit_comparator : int) (with_explicit_comparator : int)];
  [%expect
    {|
    ((without_explicit_comparator 6)
     (with_explicit_comparator    3))
    |}];
  Incr.Observer.disallow_future_use o1;
  Incr.Observer.disallow_future_use o2
;;
