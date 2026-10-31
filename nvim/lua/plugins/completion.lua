local M = {}

function M.setup()
  local ok_blink, blink = pcall(require, "blink.cmp")
  if not ok_blink then return end

  local sql_sources = require("user").private.features.database ~= false and { "dadbod", "buffer", "snippets" }
    or { "lsp", "buffer", "snippets" }

  blink.setup {
    appearance = {
      nerd_font_variant = "mono",
    },
    completion = {
      documentation = { auto_show = true },
      list = { selection = { preselect = false, auto_insert = false } },
      accept = {
        auto_brackets = {
          enabled = true,
        },
      },
    },
    fuzzy = {
      implementation = "prefer_rust",
    },
    keymap = {
      preset = "default",
      ["<C-space>"] = { "show", "show_documentation", "hide_documentation" },
      ["<CR>"] = { "select_and_accept", "fallback" },
      ["<Tab>"] = { "select_next", "snippet_forward", "fallback" },
      ["<S-Tab>"] = { "select_prev", "snippet_backward", "fallback" },
    },
    snippets = {
      preset = "default",
    },
    sources = {
      default = { "lsp", "path", "snippets", "buffer" },
      per_filetype = {
        sql = sql_sources,
        mysql = sql_sources,
        plsql = sql_sources,
      },
      providers = {
        dadbod = { name = "Dadbod", module = "plugins.dadbod_source" },
      },
    },
  }
end

return M
