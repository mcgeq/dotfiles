local M = {}
M.version = "v0.1.0"

---@type UserConfig
M.config = require("mcge.core.config")
--- @param user_config UserConfig
function M.setup(user_config)
	require("mcge.utils.global")
	require("mcge.etc")
end

return M
