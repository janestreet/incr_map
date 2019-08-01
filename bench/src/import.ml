module Incr = Incremental.Make ()
module Incr_map = Incr_map.Make (Incr)
module Var = Incr.Var
module Obs = Incr.Observer
module Bench = Core_bench.Bench
include Incr.Let_syntax

module Infix = struct
  let ( := ) = Var.set
  let ( ! ) = Var.value
end
