module Incr = Incremental.Make ()
module Incr_map = Incr_map.Make (Incr)
module Var = Incr.Var
module Obs = Incr.Observer
module Bench = Core_bench.Std.Bench
include Incr.Let_syntax

let ( := ) = Var.set
let ( ! ) = Var.value
