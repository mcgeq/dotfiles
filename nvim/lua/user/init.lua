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
  lang = {},
  private = {
    -- Machine-level feature toggles. All keys default to enabled; set one to
    -- `false` in `user.private` (git-ignored) to turn that feature off on
    -- this machine. See `private.example.lua` for the full reference.
    features = {
      dashboard = true, -- snacks dashboard 启动页
      bufferline = true, -- bufferline 顶部标签栏
      noice = true, -- noice 消息/命令行 UI
      nerd_font = true, -- 使用 Nerd Font 图标
      debug = true, -- DAP 调试 (codelldb / debugpy)
      database = true, -- dadbod 数据库 UI
      go = true, -- go.nvim 工具链
      cpp = true, -- clangd_extensions + cmake-tools
      frontend = true, -- 前端 eslint / colorizer / autotag
      live_server = true, -- live-server 终端
      web = true, -- package-info + hurl
      spectre = true, -- nvim-spectre 批量替换
      markdown_extras = true, -- markdown 表格/图片/TOC/render
      format_on_save = true, -- 保存时自动格式化
    },
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
merge_section("lang", "user.lang")

-- `user.private` (git-ignored) is the machine-level layer: it overrides every
-- section above and supplies the `features` toggles.
local private_override_sections = { "theme", "pack", "session", "lsp", "lang" }

local private = util.require_if_exists("user.private")
if private ~= nil then
  for _, key in ipairs(private_override_sections) do
    if private[key] ~= nil then
      M[key] = vim.tbl_deep_extend("force", vim.deepcopy(M[key] or {}), private[key])
    end
  end
end
M.private = vim.tbl_deep_extend("force", vim.deepcopy(DEFAULTS.private or {}), private or {})

return M
