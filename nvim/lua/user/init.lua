local M = {
  colorscheme = nil,
  theme = {
    -- Preferred colorscheme. If nil, falls back to `colorscheme` then "tokyonight".
    name = nil,
    -- Example:
    -- name = "kanagawa",
    -- Override per-theme setup without editing base config.
    overrides = {
      -- kanagawa = {
      --   theme = "dragon",
      --   transparent = true,
      -- },
    },
    -- Optional highlight overrides. Can be a table or function(theme_name).
    -- Example:
    -- highlights = {
    --   NormalFloat = { bg = "NONE" },
    --   CursorLineNr = { bold = true },
    -- },
    highlights = nil,
  },
  pack = {
    -- Add base plugin names here to turn them off.
    disable = {},
    -- Add raw vim.pack specs here for small customizations.
    add = {},
  },
  lsp = {
    -- Add Mason-backed server names here, for example: { "clangd" }.
    ensure_installed = {},
    ensure_installed_lookup = {},
    -- Add or override server configs here, keyed by server name.
    servers = {},
  },
}

for _, name in ipairs(M.lsp.ensure_installed) do
  M.lsp.ensure_installed_lookup[name] = true
end

return M
