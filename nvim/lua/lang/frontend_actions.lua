local M = {}

local FRONTEND_CLIENTS = { "eslint", "vtsls", "vue_ls" }

function M.has_client(bufnr, names)
  local lookup = {}
  for _, name in ipairs(names or {}) do
    lookup[name] = true
  end

  for _, client in ipairs(vim.lsp.get_clients({ bufnr = bufnr })) do
    if lookup[client.name] then return true end
  end

  return false
end

function M.show_frontend_clients(bufnr)
  local names = {}

  for _, client in ipairs(vim.lsp.get_clients({ bufnr = bufnr })) do
    if vim.list_contains(FRONTEND_CLIENTS, client.name) then
      table.insert(names, client.name)
    end
  end

  table.sort(names)
  vim.notify("Frontend LSP: " .. (#names > 0 and table.concat(names, ", ") or "none"), vim.log.levels.INFO, {
    title = "Frontend Clients",
  })
end

function M.apply_sync_code_action(bufnr, only, client_names)
  local allowed = {}
  for _, name in ipairs(client_names or {}) do
    allowed[name] = true
  end

  local params = vim.lsp.util.make_range_params(nil, "utf-8")
  params.context = {
    only = { only },
    diagnostics = vim.diagnostic.get(bufnr),
    triggerKind = vim.lsp.protocol.CodeActionTriggerKind.Automatic,
  }

  for _, client in ipairs(vim.lsp.get_clients({ bufnr = bufnr })) do
    if not client_names or allowed[client.name] then
      local response = client:request_sync("textDocument/codeAction", params, 1200, bufnr)
      local actions = response and response.result or nil
      if actions and not vim.tbl_isempty(actions) then
        for _, action in ipairs(actions) do
          if action.edit then
            vim.lsp.util.apply_workspace_edit(action.edit, client.offset_encoding or "utf-8")
          end
          if action.command then
            local command = type(action.command) == "table" and action.command or action
            pcall(vim.lsp.buf.execute_command, command)
          end
        end
        return true
      end
    end
  end

  return false
end

function M.preferred_code_action(only, client_names)
  return function()
    local bufnr = vim.api.nvim_get_current_buf()

    if only == "source.fixAll.eslint" and vim.fn.exists(":EslintFixAll") == 2 and M.has_client(bufnr, { "eslint" }) then
      pcall(vim.cmd, "silent EslintFixAll")
      return
    end

    if M.apply_sync_code_action(bufnr, only, client_names) then return end

    vim.lsp.buf.code_action({
      apply = true,
      context = {
        only = { only },
        diagnostics = vim.diagnostic.get(bufnr),
      },
    })
  end
end

function M.run_save_actions(bufnr, frontend)
  local save_actions = frontend.save_actions or {}

  if frontend.eslint_fix_on_save == false then return end

  if save_actions.eslint and M.has_client(bufnr, { "eslint" }) then
    if vim.fn.exists(":EslintFixAll") == 2 then
      pcall(vim.cmd, "silent EslintFixAll")
    else
      M.apply_sync_code_action(bufnr, "source.fixAll.eslint", { "eslint" })
    end
  end

  if save_actions.organize_imports and M.has_client(bufnr, { "vtsls" }) then
    M.apply_sync_code_action(bufnr, "source.organizeImports", { "vtsls" })
  end

  if save_actions.remove_unused and M.has_client(bufnr, { "vtsls" }) then
    M.apply_sync_code_action(bufnr, "source.removeUnused.ts", { "vtsls" })
  end

  if save_actions.add_missing_imports and M.has_client(bufnr, { "vtsls" }) then
    M.apply_sync_code_action(bufnr, "source.addMissingImports.ts", { "vtsls" })
  end
end

return M
