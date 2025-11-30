-- 配置加载器工具
-- 提供安全的模块加载和配置合并功能

local M = {}

--- 安全地加载模块
---@param module_name string 模块名称
---@return any|nil module 加载的模块或 nil
function M.safe_require(module_name)
  local ok, module = pcall(require, module_name)
  if not ok then
    vim.notify(
      string.format("Failed to load module: %s\nError: %s", module_name, module),
      vim.log.levels.WARN
    )
    return nil
  end
  return module
end

--- 加载配置文件，如果失败则返回默认值
---@param config_name string 配置名称（不含 "config." 前缀）
---@param default any|nil 默认值
---@return any config 配置对象
function M.load_config(config_name, default)
  local module = M.safe_require("config." .. config_name)
  return module or default or {}
end

--- 深度合并两个表
---@param base table 基础表
---@param override table 覆盖表
---@return table merged 合并后的表
function M.deep_merge(base, override)
  local result = vim.deepcopy(base)
  for k, v in pairs(override) do
    if type(v) == "table" and type(result[k]) == "table" then
      result[k] = M.deep_merge(result[k], v)
    else
      result[k] = v
    end
  end
  return result
end

return M
