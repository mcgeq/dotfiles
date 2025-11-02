-- 自动更新时间戳工具函数
-- 在保存文件前自动更新 "Last Modified: " 行的时间戳

local M = {}

-- 自动更新最后修改时间戳
function M.auto_update_last_update_info()
  -- 检查缓冲区是否被修改，只有修改过的文件才更新时间戳
  -- 注意：BufWritePre 事件中 modified 通常为 true，但添加此检查更安全
  if not vim.api.nvim_buf_get_option(0, "modified") then
    return
  end

  -- 保存原始光标位置
  local origin_pos = vim.api.nvim_win_get_cursor(0)
  -- 要查找的正则表达式
  local regexp = " Last Modified:  "
  -- 获取当前缓冲区的内容
  local lines = vim.api.nvim_buf_get_lines(0, 0, -1, false)

  -- 查找包含 " Last Modified:  " 的行
  for i, line in ipairs(lines) do
    local start_pos = line:find(regexp, 1, true) -- 使用 plain search
    if start_pos then
      -- 检查时间戳部分是否已经是最新的（可选，如果不想频繁更新时间戳可以启用）
      -- 这里我们总是更新，因为已经通过 modified 检查过滤了无变更的文件
      local prefix = line:sub(1, start_pos + #regexp - 1)
      local timestamp = os.date "%Y-%m-%d %H:%M:%S"
      local new_line = prefix .. timestamp

      -- 只有时间戳部分不同时才更新（避免不必要的修改）
      if line ~= new_line then
        -- 更新该行
        vim.api.nvim_buf_set_lines(0, i - 1, i, false, { new_line })
      end

      -- 恢复光标位置
      vim.api.nvim_win_set_cursor(0, origin_pos)
      break
    end
  end
end

-- 设置自动更新命令
function M.setup()
  -- 创建 autocmd 组
  local augroup = vim.api.nvim_create_augroup("AutoUpdateTimestamp", { clear = true })

  -- 为指定文件类型设置 BufWritePre 事件
  vim.api.nvim_create_autocmd("BufWritePre", {
    group = augroup,
    pattern = { "*.rs", "*.c", "*.cpp", "*.py", "*.ts", "*.cs" },
    callback = function() M.auto_update_last_update_info() end,
    desc = "Auto update Last Modified timestamp",
  })
end

return M
