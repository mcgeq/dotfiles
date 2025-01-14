local M = {}

M.keyAttach = function(bufnr)
  local lsp = require("pisces").config.lsp
  if not lsp then
    return
  end

  local opt = { noremap = true, silent = true, buffer = bufnr }

  ------ Diagnostic
  -- Show diagnostics in a floating window.
  keymap("n", lsp.open_float, vim.diagnostic.open_float, {desc = 'Diagnostic open'})
  -- Move to the next diagnostic.
  keymap("n", lsp.goto_next, vim.diagnostic.goto_next, { desc = 'Diagnostic next'})
  -- Move to the previous diagnostic.
  keymap("n", lsp.goto_prev, vim.diagnostic.goto_prev, { desc = 'Diagnostic prev'})

  ------ LSP
  local telescope_builtin = require("telescope.builtin")
  -- Goto the definition of the word under the cursor, if there's only one, otherwise show all options in Telescope
  keymap("n", lsp.definition, telescope_builtin.lsp_definitions, opt, {desc = 'Telescope lsp_definitions'})
  -- Goto the implementation of the word under the cursor if there's only one, otherwise show all options in Telescope
  keymap("n", lsp.implementation, telescope_builtin.lsp_implementations, opt, { desc = 'Telescope lsp_implementations'})
  -- Lists LSP references for word under the cursor
  keymap("n", lsp.references, function()
    telescope_builtin.lsp_references(require("telescope.themes").get_ivy())
  end, opt)
  -- Displays hover information
  keymap("n", lsp.hover, function()
    -- nvim-ufo
    local ufo = pRequire("ufo")
    if ufo then
      local winid = ufo.peekFoldedLinesUnderCursor()
      if not winid then
        vim.lsp.buf.hover()
      end
    else
      vim.lsp.buf.hover()
    end
  end, opt, {desc = 'VIM lsp hover'})
  -- Rename variable under the cursor
  keymap("n", lsp.rename, vim.lsp.buf.rename, opt, { desc = 'VIM lsp buf rename' })
  keymap("n", lsp.code_action, vim.lsp.buf.code_action, opt, { desc = 'VIM lsp code_action' })
  keymap("n", lsp.format, function()
    vim.lsp.buf.format({ async = true })
  end, opt)
  keymap("n", lsp.call_in, telescope_builtin.lsp_incoming_calls, { desc = 'Telescope lsp_incoming_calls' })
  keymap("n", lsp.call_out, telescope_builtin.lsp_outgoing_calls, { desc = 'Telescope lsp_outgoing_calls'})
end

-- 禁用格式化功能，交给专门插件插件处理
M.disableFormat = function(client)
  if vim.fn.has("nvim-0.8") == 1 then
    client.server_capabilities.documentFormattingProvider = false
    client.server_capabilities.documentRangeFormattingProvider = false
  else
    client.resolved_capabilities.document_formatting = false
    client.resolved_capabilities.document_range_formatting = false
  end
end

-- M.capabilities = require("cmp_nvim_lsp").update_capabilities(vim.lsp.protocol.make_client_capabilities())
M.capabilities = require("cmp_nvim_lsp").default_capabilities()
-- nvim-ufo fold
M.capabilities.textDocument.foldingRange = {
  dynamicRegistration = false,
  lineFoldingOnly = true,
}

M.flags = {
  debounce_text_changes = 150,
}

return M
