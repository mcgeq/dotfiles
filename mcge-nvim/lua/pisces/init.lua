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

local function get_git_user_info()
	local username = vim.fn.systemlist("git config user.name")
	local email = vim.fn.systemlist("git config user.email")
	-- 如果没有获取到信息，默认返回空字符
	local u = #username > 0 and username[1] or "mcgeq"
	local e = #email > 0 and email[1] or "mcgeq@outlook.com"
	return u, e
end

-- 自动更新 "Last Modified" 信息
local function update_last_modified_info()
	local origin_pos = vim.fn.getpos(".") -- 获取当前光标位置
	local regexp = " Last Modified:  "
	local line_num, col_num = unpack(vim.fn.searchpos(regexp, "n")) -- 查找 "Last Modified: " 字符串的位置

	-- 如果找到了该行
	if line_num ~= 0 then
		local line = vim.fn.getline(line_num) -- 获取这一行的内容
		local str_len = col_num + #regexp -- 计算 "Last Modified: " 后面内容的开始位置
		local updated_line = line:sub(1, str_len - 1) .. vim.fn.strftime("%Y-%m-%d %H:%M:%S") -- 更新为当前时间

		vim.fn.setline(line_num, updated_line) -- 设置更新后的内容
	end

	local username, email = get_git_user_info()
	local regexp_modified_by = " Modified   By:  "
	local line_num_modified_by, col_num_modified_by = unpack(vim.fn.searchpos(regexp_modified_by, "n")) -- 查找 "Modified By: " 字符串的位置

	-- 如果找到了该行，更新 "Modified By" 字段
	if line_num_modified_by ~= 0 then
		local line_modified_by = vim.fn.getline(line_num_modified_by) -- 获取这一行的内容
		local str_len_modified_by = col_num_modified_by + #regexp_modified_by
		local updated_line_modified_by = line_modified_by:sub(1, str_len_modified_by - 1)
			.. username
			.. " <"
			.. email
			.. ">" -- 更新为当前 Git 用户

		-- 更新 "Modified By: " 信息
		vim.fn.setline(line_num_modified_by, updated_line_modified_by) -- 设置更新后的内容
	end

	vim.fn.setpos(".", origin_pos) -- 恢复光标位置
end

-- 设置在文件保存时自动调用更新 "Last Modified" 信息
vim.api.nvim_create_autocmd("BufWritePre", {
	pattern = "*.{rs,c,cpp,py,ts,cs}", -- 所有文件类型
	callback = update_last_modified_info,
})

return M
