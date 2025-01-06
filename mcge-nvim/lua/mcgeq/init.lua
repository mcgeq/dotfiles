-- Starting
local M = {}
M.version = "v0.1.0"

function M.setup()
	local utils = require("mcgeq.utils")
	local expected_version = "0.10.3"
	utils.mg_is_compatible_version(expected_version)

  require("mcgeq.core")
  require("mcgeq.internal")
end

return M
