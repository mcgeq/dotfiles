-- 自动更新时间戳工具函数
-- 在保存文件前自动更新 "Last Modified: " 行的时间戳

local M = {}

-- 自动更新最后修改时间戳
function M.auto_update_last_update_info()
  -- 检查缓冲区是否被修改，只有修改过的文件才更新时间戳
  -- 注意：BufWritePre 事件中 modified 通常为 true，但添加此检查更安全
  -- 使用推荐的方式检查缓冲区修改状态
  local is_modified = vim.api.nvim_get_option_value("modified", { buf = 0 })
  if not is_modified then return end

  -- 保存原始光标位置
  local origin_pos = vim.api.nvim_win_get_cursor(0)
  -- 要查找的正则表达式（支持不同注释格式）
  local patterns = {
    " Last Modified:  ", -- 标准格式（// Last Modified: 或 # Last Modified: 等）
    " Last Modified:", -- 无多余空格
    "Last Modified:  ", -- 无前置空格
    "Last Modified:", -- 最简格式
  }
  -- 获取当前缓冲区的内容
  local lines = vim.api.nvim_buf_get_lines(0, 0, -1, false)

  -- 查找包含 "Last Modified: " 的行（支持多种格式）
  for i, line in ipairs(lines) do
    for _, pattern in ipairs(patterns) do
      local start_pos = line:find(pattern, 1, true) -- 使用 plain search
      if start_pos then
        -- 检查时间戳部分是否已经是最新的
        local prefix = line:sub(1, start_pos + #pattern - 1)
        local timestamp = os.date "%Y-%m-%d %H:%M:%S"
        local new_line = prefix .. timestamp

        -- 只有时间戳部分不同时才更新（避免不必要的修改）
        if line ~= new_line then
          -- 更新该行
          vim.api.nvim_buf_set_lines(0, i - 1, i, false, { new_line })
        end

        -- 恢复光标位置
        vim.api.nvim_win_set_cursor(0, origin_pos)
        return -- 找到并处理完成后退出
      end
    end
  end
end

-- 设置自动更新命令
function M.setup()
  -- 创建 autocmd 组
  local augroup = vim.api.nvim_create_augroup("AutoUpdateTimestamp", { clear = true })

  -- 为指定文件类型设置 BufWritePre 事件（扩展支持更多文件类型）
  vim.api.nvim_create_autocmd("BufWritePre", {
    group = augroup,
    pattern = {
      "*.rs", -- Rust
      "*.c", -- C
      "*.cpp", -- C++
      "*.h", -- C/C++ Header
      "*.hpp", -- C++ Header
      "*.py", -- Python
      "*.ts", -- TypeScript
      "*.tsx", -- TypeScript React
      "*.js", -- JavaScript
      "*.jsx", -- JavaScript React
      "*.cs", -- C#
      "*.java", -- Java
      "*.go", -- Go
      "*.lua", -- Lua
      "*.css", -- CSS
      "*.scss", -- SCSS
      "*.html", -- HTML
    },
    callback = function() M.auto_update_last_update_info() end,
    desc = "Auto update Last Modified timestamp",
  })
end

return M
