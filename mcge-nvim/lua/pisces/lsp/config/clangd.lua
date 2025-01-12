local common = require("pisces.lsp.common-config")
common.capabilities.offsetEncoding = "utf-8"

local opts = {
	capabilities = common.capabilities,
	flags = common.flags,
	cmd = {
		"clangd",
		"--all-scopes-completion",
		"--background-index",
		"--clang-tidy",
		"--completion-style=detailed",
		"--enable-config",
		"--pch-storage=memory",
		"--completion-parse=auto",
		"--function-arg-placeholders=true",
		"--header-insertion-decorators",
	},
	init_options = {
		clangdFileStatus = true,
		usePlaceholders = true,
		completeUnimported = true,
		semanticHighlighting = true,
		fallbackFlags = { "-std=c++20" },
	},
	on_attach = function(client, bufnr)
		common.disableFormat(client)
		common.keyAttach(bufnr)
	end,
}

return {
	on_setup = function(server)
		server.setup(opts)
	end,
}
