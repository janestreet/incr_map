open! Core
open! Import

let () =
  Command.run
    (Command.group
       ~summary:"Incremental map benchmarks"
       [ "sum", Sum.command
       ; "nested-sum", Nested_sum.command
       ; "shares-per-symbol", Shares_per_symbol.command
       ])
;;
