include Expect_test_helpers_kernel.Std
module Incr = struct
  module Z = Incremental_kernel.Std.Incremental.Make()
  include Z
  module Map = Incr_map.Make(Z)
end
include Incr.Let_syntax
