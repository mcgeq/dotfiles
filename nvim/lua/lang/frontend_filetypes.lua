local M = {}

M.eslint_owned = {
  css = true,
  html = true,
  javascript = true,
  javascriptreact = true,
  json = true,
  jsonc = true,
  less = true,
  scss = true,
  toml = true,
  typescript = true,
  typescriptreact = true,
  vue = true,
  yaml = true,
}

M.visual_filetypes = {
  "css",
  "scss",
  "sass",
  "less",
  "html",
  "javascript",
  "typescript",
  "javascriptreact",
  "typescriptreact",
  "vue",
  "svelte",
}

M.lsp_filetypes = {
  "javascript",
  "javascriptreact",
  "typescript",
  "typescriptreact",
  "vue",
}

function M.build_eslint_lookup(extra_filetypes)
  local lookup = vim.deepcopy(M.eslint_owned)
  for _, filetype in ipairs(extra_filetypes or {}) do
    lookup[filetype] = true
  end
  return lookup
end

return M
