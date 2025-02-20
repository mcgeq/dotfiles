-- DEPRECATED
local formatter = pRequire("formatter")
if not formatter then
	return
end

formatter.setup({
	filetype = {
		lua = {
			function()
				return {
					exe = "stylua",
					args = {
						-- "--config-path "
						--   .. os.getenv("XDG_CONFIG_HOME")
						--   .. "/stylua/stylua.toml",
						"-",
					},
					stdin = true,
				}
			end,
		},
		rust = {
			-- Rustfmt
			function()
				return {
					exe = "rustfmt",
					args = { "--emit=stdout" },
					stdin = true,
				}
			end,
		},
		javascript = {
			-- prettier
			function()
				return {
					exe = "prettier",
					args = { "--stdin-filepath", vim.fn.fnameescape(vim.api.nvim_buf_get_name(0)), "--single-quote" },
					stdin = true,
				}
			end,
		},
	},
})

-- format on save
vim.api.nvim_create_autocmd("BufWritePost", {
	pattern = { "*.js", "*.rs", "*.lua" },
	command = "FromatWrite",
	group = vim.api.nvim_create_augroup("FormatAutogroup", { clear = true }),
})
-- vim.api.nvim_exec(
--   [[
-- augroup FormatAutogroup
--   autocmd!
--   autocmd BufWritePost *.js,*.rs,*.lua FormatWrite
-- augroup END
-- ]],
--   true
-- )
