module Incr = struct
  module Z = Incremental_kernel.Incremental.Make()
  include Z
  module Map = Incr_map.Make(Z)
end
include Incr.Let_syntax
