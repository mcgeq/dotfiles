-- This will run last in the setup process.
-- This is just pure lua so anything that doesn't
-- fit in the normal config locations above can go here

-- 自动更新时间戳功能
local auto_update_timestamp = require("config.auto_update_timestamp")
auto_update_timestamp.setup()

-- ===== 配置优化模块 =====
-- 启用用户命令（PresetSwitch, ConfigInfo, etc.）
require("config.commands").setup()

-- 启用配置验证器（启动时自动检查配置）
require("config.validator").auto_validate()