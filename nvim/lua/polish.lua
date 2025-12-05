-- This will run last in the setup process.
-- This is just pure lua so anything that doesn't
-- fit in the normal config locations above can go here

-- ===== 自动更新时间戳功能 =====
local ok, auto_update_timestamp = pcall(require, "config.auto_update_timestamp")
if ok and auto_update_timestamp.setup then
  auto_update_timestamp.setup()
else
  vim.notify("Failed to load auto_update_timestamp", vim.log.levels.WARN)
end

-- ===== 配置优化模块 =====
-- 启用用户命令（PresetSwitch, ConfigInfo, etc.）
ok = pcall(function()
  require("config.commands").setup()
end)
if not ok then
  vim.notify("Failed to load custom commands", vim.log.levels.WARN)
end

-- 启用配置验证器（启动时自动检查配置）
ok = pcall(function()
  require("config.validator").auto_validate()
end)
if not ok then
  vim.notify("Failed to load config validator", vim.log.levels.WARN)
end