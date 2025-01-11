local common = require("pisces.lsp.common-config")
local opts = {
	capabilities = common.capabilities,
	flags = common.flags,
	on_attach = function(client, bufnr)
		common.disableFormat(client)
		common.keyAttach(bufnr)
	end,
	settings = {
		-- to enable rust-analyzer settings visit:
		-- https://github.com/rust-analyzer/rust-analyzer/blob/master/docs/user/generated_config.adoc
		["rust-analyzer"] = {
			-- enable clippy on save
			checkOnSave = {
				command = "clippy",
			},
		},
	},
}

return {
	on_setup = function(server)
		local ok_rt, rustaceanvim = pcall(require, "rustaceanvim")
		if not ok_rt then
			print("Failed to load rust tools, will set up `rust_analyzer` without `rustaceanvim`.")
			server.setup(opts)
		else
			-- We don't want to call lspconfig.rust_analyzer.setup() when using rust-tools
			rustaceanvim.setup({
				server = opts,
				-- NOTICE: require dap
				dap = require("pisces.dap.nvim-dap.config.rust"),
			})
		end
	end,
}
