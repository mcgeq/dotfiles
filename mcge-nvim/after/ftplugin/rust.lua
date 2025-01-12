local bufnr = vim.api.nvim_get_current_buf()

-- vim.keymap.set(
--    'n',
--    '<leader>a',
--    function ()
--       vim.cmd.RustLsp('codeAction')
--    end,
--    {silent = true, buffer = bufnr}
-- )
--
-- vim.keymap.set(
--   "n",
--   "K",  -- Override Neovim's built-in hover keymap with rustaceanvim's hover actions
--   function()
--     vim.cmd.RustLsp({'hover', 'actions'})
--   end,
--   { silent = true, buffer = bufnr }
-- )
keymap("n", "<leader>a", function()
	vim.cmd.RustLsp("codeAction")
end, { silent = true, buffer = bufnr })

keymap("n", "<leader>k", function()
	vim.cmd.RustLsp({ "hover", "actions" })
end, { silent = true, buffer = bufnr })

local lsp = require("pisces").config.lsp
if not lsp then
	return
end

local opt = { noremap = true, silent = true, buffer = bufnr }

local telescope_builtin = require("telescope.builtin")
-- Goto the definition of the word under the cursor, if there's only one, otherwise show all options in Telescope
keymap("n", lsp.definition, telescope_builtin.lsp_definitions, opt)
-- Goto the implementation of the word under the cursor if there's only one, otherwise show all options in Telescope
keymap("n", lsp.implementation, telescope_builtin.lsp_implementations, opt)
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
end, opt)
-- Rename variable under the cursor
keymap("n", lsp.rename, vim.lsp.buf.rename, opt)
keymap("n", lsp.code_action, vim.lsp.buf.code_action, opt)
keymap("n", lsp.format, function()
	vim.lsp.buf.format({ async = true })
end, opt)
keymap("n", lsp.call_in, telescope_builtin.lsp_incoming_calls)
keymap("n", lsp.call_out, telescope_builtin.lsp_outgoing_calls)
