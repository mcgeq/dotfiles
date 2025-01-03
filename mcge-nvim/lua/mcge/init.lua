local M = {}
M.version = "0.1.0"

---@type UserConfig
M.config = require("mcge.config")
--- @param user_config UserConfig
function M.setup(user_config)
  require("mcge.utils.global")
  require("mcge.options")
  -- user config override
  M.config = vim.tbl_deep_extend("force", M.config, user_config)
  require("mcge.keymaps")
  local pluginManager = require("mcge.lazy")
  if not pluginManager.avaliable() then
    pluginManager.install()
  end
  pluginManager.setup()
  require("mcge.colorscheme").reset()
  require("mcge.autocmds")
end
return M
