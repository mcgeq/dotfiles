local M = {}

local FRONTEND_CLIENTS = { "eslint", "vtsls", "vue_ls", "biome" }

function M.has_client(bufnr, names)
  local lookup = {}
  for _, name in ipairs(names or {}) do
    lookup[name] = true
  end

  for _, client in ipairs(vim.lsp.get_clients { bufnr = bufnr }) do
    if lookup[client.name] then return true end
  end

  return false
end

function M.show_frontend_clients(bufnr)
  local names = {}

  for _, client in ipairs(vim.lsp.get_clients { bufnr = bufnr }) do
    if vim.list_contains(FRONTEND_CLIENTS, client.name) then table.insert(names, client.name) end
  end

  table.sort(names)
  vim.notify("Frontend LSP: " .. (#names > 0 and table.concat(names, ", ") or "none"), vim.log.levels.INFO, {
    title = "Frontend Clients",
  })
end

function M.apply_sync_code_action(bufnr, only, client_names)
  return require("lsp.actions").apply(bufnr, only, client_names)
end

function M.preferred_code_action(only, client_names)
  return function()
    local bufnr = vim.api.nvim_get_current_buf()

    if only == "source.fixAll.eslint" and vim.fn.exists ":EslintFixAll" == 2 and M.has_client(bufnr, { "eslint" }) then
      pcall(vim.cmd, "silent EslintFixAll")
      return
    end

    if M.apply_sync_code_action(bufnr, only, client_names) then return end

    vim.lsp.buf.code_action {
      apply = true,
      context = {
        only = { only },
        diagnostics = vim.diagnostic.get(bufnr),
      },
    }
  end
end

function M.run_save_actions(bufnr, frontend, opts)
  opts = opts or {}
  local save_actions = frontend.save_actions or {}
  if opts.provider == "biome" then
    if opts.fix ~= false and frontend.eslint_fix_on_save ~= false and save_actions.eslint then
      require("lang.biome").fix_all(bufnr)
    end
    if opts.imports ~= false and save_actions.organize_imports then require("lang.biome").organize_imports(bufnr) end
    return
  end
  if
    (opts.provider == nil or opts.provider == "eslint")
    and opts.fix ~= false
    and frontend.eslint_fix_on_save ~= false
    and save_actions.eslint
    and M.has_client(bufnr, { "eslint" })
  then
    M.apply_sync_code_action(bufnr, "source.fixAll.eslint", { "eslint" })
  end
  if opts.imports == false then return end
  for _, action in ipairs {
    { "organize_imports", "source.organizeImports" },
    { "remove_unused", "source.removeUnused.ts" },
    { "add_missing_imports", "source.addMissingImports.ts" },
  } do
    if save_actions[action[1]] and M.has_client(bufnr, { "vtsls" }) then
      M.apply_sync_code_action(bufnr, action[2], { "vtsls" })
    end
  end
end

return M
