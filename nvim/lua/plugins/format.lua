local M = {}

local DEFAULT_FORMATTERS_BY_FT = {
  c = { "clang_format" },
  cpp = { "clang_format" },
  css = { "prettier" },
  go = { "gofumpt", "goimports" },
  html = { "prettier" },
  javascript = { "prettier" },
  javascriptreact = { "prettier" },
  json = { "prettier" },
  jsonc = { "prettier" },
  lua = { "stylua" },
  markdown = { "prettier" },
  python = { "ruff_fix", "ruff_format" },
  rust = { "rustfmt" },
  scss = { "prettier" },
  sh = { "shfmt" },
  toml = { "taplo" },
  typescript = { "prettier" },
  typescriptreact = { "prettier" },
  vue = { "prettier" },
  yaml = { "prettier" },
  zig = { "zigfmt" },
}

function M.setup()
  local map = require("core.keymaps").map
  local user = require "user"
  local user_lang = user.lang
  local formatters_by_ft =
    vim.tbl_deep_extend("force", vim.deepcopy(DEFAULT_FORMATTERS_BY_FT), user_lang.formatters_by_ft or {})
  local ok_conform, conform = pcall(require, "conform")
  if ok_conform then
    local formatters = {
      ruff_fix = {
        command = function(_, ctx) return require("lang.python").executable("ruff", ctx.filename) end,
      },
      ruff_format = {
        command = function(_, ctx) return require("lang.python").executable("ruff", ctx.filename) end,
      },
    }
    formatters = vim.tbl_deep_extend("force", formatters, user_lang.formatters or {})

    conform.setup {
      formatters_by_ft = formatters_by_ft,
      formatters = formatters,
    }
  end

  require("lang.formatting").setup()
  map(
    { "n", "v" },
    "<leader>cf",
    function() require("lang.formatting").format(vim.api.nvim_get_current_buf(), { async = true }) end,
    "Format buffer"
  )
end

return M
