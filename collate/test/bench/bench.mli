(* Simple benchmarking library. *)

(*_ Why didn't I use inline_benchmarks you might ask? Clearly a benchmarking suite would
  be better for benchmarking? Well, no. It reported totally nonsensical results, including
  negative number of words allocated and 1_000_000x time difference on benchmarks that
  actually took the same amount of time. Well, apparently fitting a line to results helps
  only with some kinds of noise, and actually amplifies others. *)

open Core

val run : verbose:bool -> name:string -> init_f:(unit -> (unit -> unit) Staged.t) -> unit
