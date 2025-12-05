-- AstroCommunity 插件配置（优化版）
-- 使用插件管理器和预设系统，提升可维护性和可扩展性
-- 
-- 预设系统使用方法：
-- 1. 使用命令：:PresetSwitch <preset_name>
-- 2. 环境变量：export NVIM_PRESET=frontend
-- 3. 配置文件：echo "frontend" > ~/.config/nvim/.preset
--
-- 旧版配置备份在 community_backup.lua

---@type LazySpec
local plugin_manager = require("config.plugin_manager")
local presets = require("config.presets")

-- 读取当前预设
local current_preset = presets.read_preset_file()
local preset_config = presets.apply(current_preset)

-- 构建插件列表
local plugins = plugin_manager.build_plugin_list(preset_config)

-- 显示当前配置信息（可通过环境变量 NVIM_SHOW_PRESET_INFO=0 禁用）
vim.defer_fn(function()
  -- 允许用户通过环境变量或全局变量禁用启动通知
  local show_info = vim.env.NVIM_SHOW_PRESET_INFO ~= "0" and vim.g.show_preset_info ~= false
  
  if show_info then
    local stats = plugin_manager.get_stats()
    vim.notify(
      string.format(
        "Nvim Config Preset: %s\n" ..
        "Language Packs: %d | Features: %d | Tools: %d",
        current_preset,
        stats.language_packs,
        stats.editor_features,
        stats.workflow_tools
      ),
      vim.log.levels.INFO
    )
  end
end, 500)

return plugins
