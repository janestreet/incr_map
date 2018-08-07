include Expect_test_helpers_kernel
module Incr = struct
  module Z = Incremental_kernel.Make()
  include Z
  module Map = Incr_map.Make(Z)
end
include Incr.Let_syntax
