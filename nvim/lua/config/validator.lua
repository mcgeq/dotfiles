-- 配置验证器
-- 在启动时检查配置一致性和潜在问题

local M = {}

--- 检查必需的外部依赖
---@return table issues 问题列表
function M.check_external_dependencies()
  local issues = {}
  
  local required_tools = {
    { name = "git", desc = "Git version control" },
    { name = "rg", desc = "Ripgrep for search" },
    { name = "fd", desc = "fd for file finding" },
  }
  
  for _, tool in ipairs(required_tools) do
    if vim.fn.executable(tool.name) == 0 then
      table.insert(issues, {
        level = "warn",
        message = string.format("%s (%s) not found", tool.desc, tool.name),
      })
    end
  end
  
  return issues
end

--- 检查插件冲突
---@return table issues 问题列表
function M.check_plugin_conflicts()
  local issues = {}
  
  -- 检查格式化工具冲突
  local formatters = {
    conform = "stevearc/conform.nvim",
    none_ls = "nvimtools/none-ls.nvim",
  }
  
  local formatter_count = 0
  for name, plugin in pairs(formatters) do
    if pcall(require, name) then
      formatter_count = formatter_count + 1
    end
  end
  
  if formatter_count > 1 then
    table.insert(issues, {
      level = "warn",
      message = "Multiple formatters detected. Consider using only one.",
    })
  end
  
  return issues
end

--- 检查配置文件完整性
---@return table issues 问题列表
function M.check_config_integrity()
  local issues = {}
  
  local required_configs = {
    "config.constants",
    "config.loader",
  }
  
  for _, config in ipairs(required_configs) do
    local ok = pcall(require, config)
    if not ok then
      table.insert(issues, {
        level = "error",
        message = string.format("Required config '%s' not found", config),
      })
    end
  end
  
  return issues
end

--- 检查性能设置
---@return table issues 问题列表
function M.check_performance()
  local issues = {}
  
  -- 检查是否启用了过多的重量级插件
  local heavy_plugins = {
    "nvim-treesitter",
    "telescope.nvim",
    "cmp",
  }
  
  -- 这里可以添加更多性能检查
  
  return issues
end

--- 运行所有检查
---@return table report 检查报告
function M.run_all_checks()
  local report = {
    external_dependencies = M.check_external_dependencies(),
    plugin_conflicts = M.check_plugin_conflicts(),
    config_integrity = M.check_config_integrity(),
    performance = M.check_performance(),
  }
  
  return report
end

--- 格式化并显示检查报告
---@param report table 检查报告
function M.display_report(report)
  local has_issues = false
  
  for category, issues in pairs(report) do
    if #issues > 0 then
      has_issues = true
      vim.notify(
        string.format("\n=== %s ===", category:gsub("_", " "):upper()),
        vim.log.levels.INFO
      )
      
      for _, issue in ipairs(issues) do
        local level = vim.log.levels[issue.level:upper()] or vim.log.levels.INFO
        vim.notify("  • " .. issue.message, level)
      end
    end
  end
  
  if not has_issues then
    vim.notify("✓ Configuration validation passed", vim.log.levels.INFO)
  end
end

--- 自动运行验证（可在 polish.lua 中调用）
function M.auto_validate()
  -- 延迟执行，避免影响启动时间
  vim.defer_fn(function()
    local report = M.run_all_checks()
    
    -- 只显示警告和错误
    local filtered_report = {}
    for category, issues in pairs(report) do
      filtered_report[category] = vim.tbl_filter(function(issue)
        return issue.level == "warn" or issue.level == "error"
      end, issues)
    end
    
    if vim.tbl_count(filtered_report) > 0 then
      M.display_report(filtered_report)
    end
  end, 1000) -- 1秒后执行
end

return M
