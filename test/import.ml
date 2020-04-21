include Expect_test_helpers_core

module Incr = struct
  module Z = Incremental.Make ()
  include Z
  module Map = Incr_map.Make (Z)
end

include Incr.Let_syntax
