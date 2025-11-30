-- Snippet 工具函数
-- 提供通用的 snippet 生成工具

local M = {}

--- 获取当前文件名
---@return string filename 文件名
function M.get_filename()
  return vim.fn.expand("%:t")
end

--- 获取当前日期时间
---@return string datetime 格式化的日期时间
function M.get_current_datetime()
  return os.date("%Y-%m-%d %H:%M:%S")
end

--- 获取当前年份
---@return string year 年份
function M.get_current_year()
  return os.date("%Y")
end

--- 创建分隔线
---@param prefix string 注释前缀
---@param length number 分隔线长度
---@return string line 分隔线
function M.create_separator(prefix, length)
  length = length or 77
  return prefix .. " " .. string.rep("-", length)
end

return M
