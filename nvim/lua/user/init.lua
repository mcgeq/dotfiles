local util = require("core.util")

local DEFAULTS = {
  colorscheme = nil,
  theme = {
    name = nil,
    overrides = {},
    highlights = nil,
  },
  pack = {
    disable = {},
    add = {},
  },
  session = {
    autoread = false,
    autowrite = true,
  },
  lsp = {
    ensure_installed = {},
    auto_install = true,
  },
}

local M = vim.deepcopy(DEFAULTS)

local function merge_section(key, module)
  local value = util.require_if_exists(module)
  if value == nil then return end
  M[key] = vim.tbl_deep_extend("force", vim.deepcopy(DEFAULTS[key]), value)
end

merge_section("theme", "user.theme")
merge_section("pack", "user.pack")
merge_section("session", "user.session")
merge_section("lsp", "user.lsp_settings")

return M
