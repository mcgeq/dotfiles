local M = {}
M.version = "0.1.0"

---@type UserConfig
M.config = require("pisces.config")
--- @param user_config UserConfig
function M.setup(user_config)
	require("pisces.utils.global")
	require("pisces.options")
	-- user config override
	M.config = vim.tbl_deep_extend("force", M.config, user_config)
	require("pisces.env").init(M.config)
	require("pisces.keymaps")
	local pluginManager = require("pisces.lazy")
	if not pluginManager.avaliable() then
		pluginManager.install()
	end
	pluginManager.setup()
	require("pisces.colorscheme").reset()
	require("pisces.autocmds")
	require("pisces.lsp")
	require("pisces.cmp")
	require("pisces.format")
	require("pisces.dap")
	require("pisces.utils.color-preview")
	if M.config.fix_windows_clipboard then
		require("utils.fix-yank")
	end
end

-- vim.api.nvim_create_autocmd("BufEnter", {
-- 	pattern = "*",
-- 	callback = function()
-- 		require("lazygit.utils").project_root_dir()
-- 	end,
-- })

return M
