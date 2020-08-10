open Core

module Measurement = struct
  type t =
    { time : Time_ns.Span.t
    ; words : int
    }
  [@@deriving fields]

  let to_string ({ time; words } : t) = sprintf !"t=%{Time_ns.Span}, mem=%d Wd" time words
end

let measure thunk =
  let start = Time_ns.now () in
  let start_mem = Gc.major_plus_minor_words () in
  let res = thunk () in
  ( res
  , Measurement.
      { time = Time_ns.diff (Time_ns.now ()) start
      ; words = Gc.major_plus_minor_words () - start_mem
      } )
;;

let report ~name meas = printf !"%s: %{Measurement}\n%!" name meas

module type Reportable = sig
  type t [@@deriving compare]

  val to_string : t -> string

  include Container.Summable with type t := t

  val scale : t -> float -> t
end

let report_results ~verbose (results : Measurement.t list) =
  let calculate_and_report
        (type a)
        ~name
        (results : a list)
        (module M : Reportable with type t = a)
    =
    let len = List.length results in
    let sum = List.sum (module M) ~f:Fn.id results in
    let sorted = List.sort results ~compare:M.compare in
    let p pct =
      let pos = float (len - 1) *. pct |> Int.of_float in
      List.nth_exn sorted pos
    in
    let min, p25, p50, p75, max = p 0., p 0.25, p 0.5, p 0.75, p 1. in
    let avg = M.scale sum (1. /. float len) in
    printf
      !"%s: min=%{M}, 25%%=%{M} 50%%=%{M}, avg=%{M}, 75%%=%{M}, max=%{M}\n"
      name
      min
      p25
      p50
      avg
      p75
      max
  in
  let len = List.length results in
  let times, mems =
    List.map ~f:(fun { time; words } -> time, words) results |> List.unzip
  in
  let total_time = List.sum (module Time_ns.Span) ~f:Fn.id times in
  if verbose then printf !"Results: %d runs in %{Time_ns.Span}\n" len total_time;
  calculate_and_report ~name:"Time" times (module Time_ns.Span);
  if verbose
  then
    calculate_and_report
      ~name:"Mem "
      mems
      (module struct
        include Int

        let scale i f = to_float i *. f |> of_float
      end)
;;

let run ~verbose ~name ~init_f =
  if verbose then printf "\n=== %s ===\n%!" name else printf "%s:\n%!" name;
  let report ~name res = if verbose then report ~name res else () in
  let quota = Time_ns.Span.of_int_sec 1 in
  let thunk, init_res = measure init_f in
  report ~name:"Initialize" init_res;
  let (), gc_res = measure Gc.compact in
  report ~name:"Compact" gc_res;
  let (), res1 = measure (unstage thunk) in
  report ~name:"First run" res1;
  let results = ref [ res1 ] in
  let elapsed = ref (Measurement.time res1) in
  while Time_ns.Span.( < ) elapsed.contents quota do
    let res = measure (unstage thunk) |> snd in
    results := res :: !results;
    elapsed := Time_ns.Span.( + ) (Measurement.time res) !elapsed
  done;
  report_results ~verbose !results
;;
