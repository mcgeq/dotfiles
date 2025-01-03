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
  require("mcge.env").init(M.config)
  require("mcge.keymaps")
  local pluginManager = require("mcge.lazy")
  if not pluginManager.avaliable() then
    pluginManager.install()
  end
  pluginManager.setup()
  require("mcge.colorscheme").reset()
  require("mcge.autocmds")
  require("mcge.lsp")
  require("mcge.cmp")
  require("mcge.format")
  require("mcge.dap")
  require("mcge.utils.color-preview")
  if M.config.fix_windows_clipboard then
    require("utils.fix-yank")
  end
end

return M
