local M = {}

function M.provider(bufnr)
  bufnr = bufnr == 0 and vim.api.nvim_get_current_buf() or bufnr
  if require("user").private.features.frontend == false then return "conform" end
  local biome = require "lang.biome"
  if biome.supports(bufnr) and biome.project_root(bufnr) and biome.has_client(bufnr) then return "biome" end
  local frontend = require("lang.frontend_state").get()
  if
    frontend.formatter ~= "conform"
    and require("lang.frontend_state").eslint_lookup()[vim.bo[bufnr].filetype]
    and #vim.lsp.get_clients { bufnr = bufnr, name = "eslint" } > 0
  then
    return "eslint"
  end
  return "conform"
end

function M.format(bufnr, opts)
  bufnr = bufnr or vim.api.nvim_get_current_buf()
  opts = opts or {}
  local provider = M.provider(bufnr)
  if provider == "biome" then
    require("lang.biome").format(bufnr, opts)
  elseif provider == "eslint" then
    require("lang.frontend_actions").apply_sync_code_action(bufnr, "source.fixAll.eslint", { "eslint" })
  else
    local ok, conform = pcall(require, "conform")
    if ok then
      conform.format(vim.tbl_extend("force", { bufnr = bufnr, timeout_ms = 1000, lsp_format = "fallback" }, opts))
    else
      vim.lsp.buf.format(vim.tbl_extend("force", { bufnr = bufnr, timeout_ms = 1000 }, opts))
    end
  end
end

function M.on_save(bufnr)
  local features = require("user").private.features
  if features.format_on_save == false then return false end
  if vim.bo[bufnr].buftype ~= "" or not vim.bo[bufnr].modifiable then return false end
  local frontend = require("lang.frontend_state").get()
  local provider = M.provider(bufnr)
  if features.frontend ~= false then
    require("lang.frontend_actions").run_save_actions(bufnr, frontend, {
      provider = provider,
      fix = features.fix_on_save ~= false,
      imports = features.imports_on_save ~= false,
    })
  end
  if provider ~= "eslint" then M.format(bufnr, { async = false }) end
  return true
end

function M.setup()
  vim.api.nvim_create_autocmd("BufWritePre", {
    group = vim.api.nvim_create_augroup("lang_formatting", { clear = true }),
    callback = function(event) M.on_save(event.buf) end,
  })
end

return M
