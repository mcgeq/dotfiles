-- 自动更新时间戳工具函数
-- 在保存文件前自动更新 "Last Modified: " 行的时间戳（优化版本）

local constants = require("config.constants")

local M = {}

-- 配置
local config = {
  patterns = {
    " Last Modified:  ", -- 标准格式（// Last Modified: 或 # Last Modified: 等）
    " Last Modified:", -- 无多余空格
    "Last Modified:  ", -- 无前置空格
    "Last Modified:", -- 最简格式
  },
  max_search_lines = 50, -- 只搜索文件前 50 行，提高性能
}

--- 更新时间戳
---@param bufnr number|nil 缓冲区号，nil 表示当前缓冲区
function M.update_timestamp(bufnr)
  bufnr = bufnr or vim.api.nvim_get_current_buf()

  -- 检查文件类型是否支持
  local ft = vim.bo[bufnr].filetype
  if not vim.tbl_contains(constants.timestamp_filetypes, ft) then return end

  -- 检查缓冲区是否被修改
  if not vim.bo[bufnr].modified then return end

  -- 保存视图（更好的方式保存光标和滚动位置）
  local view = vim.fn.winsaveview()

  -- 只获取文件开头部分（文件头通常在前面）
  local line_count = vim.api.nvim_buf_line_count(bufnr)
  local search_lines = math.min(line_count, config.max_search_lines)
  local lines = vim.api.nvim_buf_get_lines(bufnr, 0, search_lines, false)

  -- 查找并更新时间戳
  for i, line in ipairs(lines) do
    for _, pattern in ipairs(config.patterns) do
      local pos = line:find(pattern, 1, true) -- plain search
      if pos then
        local prefix = line:sub(1, pos + #pattern - 1)
        local timestamp = os.date("%Y-%m-%d %H:%M:%S")
        local new_line = prefix .. timestamp

        -- 只有时间戳不同时才更新
        if line ~= new_line then
          vim.api.nvim_buf_set_lines(bufnr, i - 1, i, false, { new_line })
        end

        -- 恢复视图
        vim.fn.winrestview(view)
        return
      end
    end
  end
end

--- 设置自动更新命令
function M.setup()
  -- 创建 autocmd 组
  local augroup = vim.api.nvim_create_augroup("AutoUpdateTimestamp", { clear = true })

  -- 使用文件类型而非模式匹配（更高效）
  vim.api.nvim_create_autocmd("BufWritePre", {
    group = augroup,
    callback = function(args) M.update_timestamp(args.buf) end,
    desc = "Auto update Last Modified timestamp",
  })
end

return M
