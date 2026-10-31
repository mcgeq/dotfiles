local M = {}

-- Source actions run before writing, so resolve and execute on the same client.
function M.apply(bufnr, kind, names)
  local allowed = require("core.util").to_lookup(names)
  for _, client in ipairs(vim.lsp.get_clients { bufnr = bufnr }) do
    if not names or allowed[client.name] then
      local params = vim.api.nvim_buf_call(
        bufnr,
        function() return vim.lsp.util.make_range_params(nil, client.offset_encoding or "utf-16") end
      )
      params.context = { only = { kind }, diagnostics = {}, triggerKind = 2 }
      local response = client:request_sync("textDocument/codeAction", params, 1200, bufnr)
      local applied = false
      for _, action in ipairs(response and response.result or {}) do
        if not action.disabled then
          if action.data and not action.edit and not action.command and client:supports_method "codeAction/resolve" then
            local resolved = client:request_sync("codeAction/resolve", action, 1200, bufnr)
            action = resolved and resolved.result or action
          end
          if action.edit then
            vim.lsp.util.apply_workspace_edit(action.edit, client.offset_encoding or "utf-16")
            applied = true
          end
          if action.command then
            local command = type(action.command) == "table" and action.command or action
            local result = client:request_sync("workspace/executeCommand", command, 1200, bufnr)
            applied = applied or (result ~= nil and result.err == nil)
          end
        end
      end
      if applied then return true end
    end
  end
  return false
end

return M
