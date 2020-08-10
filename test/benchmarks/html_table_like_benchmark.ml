open Core
open Import

(* This benchmark emulates Incr.Map operation patterns that occur when exploring a big,
   changing in realtime, html table built using incremental virtual DOM. It loops for
   a given time performing a random action every time and recomputing the 'view'.
   A random action is one of:
   + changing a single row
   + scrolling up/down a fixed, small number of lines
   + moving half a page up/down
   + going to the top or bottom of the table
*)
module Generator = Quickcheck.Generator

let global_num_rows = 200000
let global_range_length = 200
let global_string_length = 20
let global_scroll_length = 10
let global_seed_for_benchmarks = "qwertyuiop"

type t =
  { rows : string Int.Map.t Incr.Var.t
  ; num_rows : int
  ; string_length : int
  ; range_length : int
  ; scroll_length : int
  ; range_begin : int Incr.Var.t
  ; view : string Int.Map.t Incr.Observer.t
  }

type direction =
  | Up
  | Down

type action =
  | Update_cell of (int * string) list
  | Scroll of direction
  | Page_move of direction
  | Top_bottom_move of direction

let create_view rows range_begin range_length =
  let range =
    Incr.map range_begin ~f:(fun range_begin ->
      Some (Incl range_begin, Incl (range_begin + range_length)))
  in
  Incr.observe (Incr.Map.subrange rows range)
;;

let create ~num_rows ~range_length ~string_length ~scroll_length =
  let rows = List.init num_rows ~f:(fun i -> i, String.make string_length 'a') in
  let rows = Incr.Var.create (Int.Map.of_alist_exn rows) in
  let range_begin = Incr.Var.create (num_rows / 2) in
  { rows
  ; num_rows
  ; string_length
  ; range_length
  ; scroll_length
  ; range_begin
  ; view = create_view (Incr.Var.watch rows) (Incr.Var.watch range_begin) range_length
  }
;;

let generate_view t =
  Incr.stabilize ();
  Incr.Observer.value_exn t.view
;;

let cell_update t updates =
  let old_value = Incr.Var.latest_value t.rows in
  let new_value =
    List.fold updates ~init:old_value ~f:(fun rows (key, data) -> Map.set rows ~key ~data)
  in
  Incr.Var.set t.rows new_value;
  generate_view t
;;

let move_begin t new_begin =
  let new_begin = Int.max 0 new_begin |> Int.min (t.num_rows - t.range_length) in
  Incr.Var.set t.range_begin new_begin;
  generate_view t
;;

let move t direction abs_length =
  let length =
    match direction with
    | Up -> abs_length
    | Down -> -abs_length
  in
  let new_begin = length + Incr.Var.latest_value t.range_begin in
  move_begin t new_begin
;;

let scroll t direction = move t direction t.scroll_length
let page_move t direction = move t direction (t.range_length / 2)
let top_bottom_move t direction = move t direction t.num_rows

let handle_action t = function
  | Update_cell updates -> cell_update t updates
  | Scroll direction -> scroll t direction
  | Page_move direction -> page_move t direction
  | Top_bottom_move direction -> top_bottom_move t direction
;;

let cell_update_generator t cell_update_batch_size =
  let tuple_gen =
    Generator.tuple2
      (Int.gen_incl 0 (t.num_rows - 1))
      (String.gen_with_length t.string_length Char.quickcheck_generator)
  in
  let list_gen = List.gen_with_length cell_update_batch_size tuple_gen in
  Generator.map list_gen ~f:(fun x -> Update_cell x)
;;

let create_action_generator
      t
      ~scroll_weight
      ~page_move_weight
      ~top_bottom_move_weight
      ~cell_update_weight
      ~cell_update_batch_size
  =
  let direction_generator = Generator.doubleton Up Down in
  Generator.weighted_union
    [ scroll_weight, Generator.map direction_generator ~f:(fun dir -> Scroll dir)
    ; page_move_weight, Generator.map direction_generator ~f:(fun dir -> Page_move dir)
    ; ( top_bottom_move_weight
      , Generator.map direction_generator ~f:(fun dir -> Top_bottom_move dir) )
    ; cell_update_weight, cell_update_generator t cell_update_batch_size
    ]
;;

let for_perf =
  let open Command.Let_syntax in
  let%map_open duration_string =
    flag
      "duration"
      (required string)
      ~doc:"TIMESPAN time for which the benchmark will run"
  and scroll_weight =
    flag "scroll-weight" (required float) ~doc:"FLOAT weight of a scroll event"
  and page_move_weight =
    flag
      "page-move-weight"
      (required float)
      ~doc:"FLOAT weight of a pageup/pagedown event"
  and top_bottom_move_weight =
    flag
      "top-bottom-move-weight"
      (required float)
      ~doc:"FLOAT weight of a 'go to top/bottom' event"
  and cell_update_weight =
    flag
      "cell-update-weight"
      (required float)
      ~doc:"FLOAT weight of a table cell update event"
  and cell_update_batch_size =
    flag
      "batch-size-of-cell-update"
      (required int)
      ~doc:"INT number of cell updates in one cell update event"
  and seed =
    flag
      "seed"
      (optional string)
      ~doc:"STRING seed to use for RNG, will use one from the OS if not provided"
  in
  fun () ->
    let seed =
      match seed with
      | None -> `Nondeterministic
      | Some seed -> `Deterministic seed
    in
    let t =
      create
        ~num_rows:global_num_rows
        ~range_length:global_range_length
        ~string_length:global_string_length
        ~scroll_length:global_scroll_length
    in
    let action_generator =
      create_action_generator
        t
        ~scroll_weight
        ~page_move_weight
        ~top_bottom_move_weight
        ~cell_update_weight
        ~cell_update_batch_size
    in
    let actions = Quickcheck.random_sequence ~seed action_generator in
    let duration = Time.Span.of_string duration_string in
    let started_at = Time.now () in
    Sequence.delayed_fold actions ~init:() ~finish:Fn.id ~f:(fun () action ~k ->
      if Time.Span.( > ) (Time.diff (Time.now ()) started_at) duration
      then ()
      else (
        let (_ : string Int.Map.t) = handle_action t action in
        k ()))
;;

let for_perf_cmd =
  Command.basic for_perf ~summary:"Look into the ml file for a description."
;;

let%bench_module "inline_benchmarks" =
  (module struct
    let setup
          ?(scroll_weight = 0.)
          ?(page_move_weight = 0.)
          ?(top_bottom_move_weight = 0.)
          ?(cell_update_weight = 0.)
          ?(cell_update_batch_size = 0)
          ()
      =
      let t =
        create
          ~num_rows:global_num_rows
          ~range_length:global_range_length
          ~string_length:global_string_length
          ~scroll_length:global_scroll_length
      in
      let action_generator =
        create_action_generator
          t
          ~scroll_weight
          ~page_move_weight
          ~top_bottom_move_weight
          ~cell_update_weight
          ~cell_update_batch_size
      in
      t, action_generator
    ;;

    let benchmark (t, action_generator) num_actions =
      let seed = `Deterministic global_seed_for_benchmarks in
      let actions = Quickcheck.random_sequence ~seed action_generator in
      let actions = Sequence.take actions num_actions |> Sequence.force_eagerly in
      fun () ->
        Sequence.iter actions ~f:(fun action ->
          let (_ : string Int.Map.t) = handle_action t action in
          ())
    ;;

    let%bench_fun "scroll" = benchmark (setup ~scroll_weight:1. ()) 350
    let%bench_fun "page-move" = benchmark (setup ~page_move_weight:1. ()) 70

    let%bench_fun "top-bottom-jumping" =
      benchmark (setup ~top_bottom_move_weight:1. ()) 500
    ;;

    let%bench_fun "cell-update" =
      benchmark (setup ~cell_update_weight:1. ~cell_update_batch_size:50 ()) 10
    ;;
  end)
;;
