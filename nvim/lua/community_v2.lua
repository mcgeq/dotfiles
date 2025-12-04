-- AstroCommunity 插件配置 v2（改进版）
-- 使用插件管理器和预设系统，提升可维护性和可扩展性
-- 
-- 使用方法：
-- 1. 在 lazy_setup.lua 中将 { import = "community" } 改为 { import = "community_v2" }
-- 2. 设置预设：在终端运行 `export NVIM_PRESET=frontend`（或其他预设）
-- 3. 或创建 ~/.config/nvim/.preset 文件，内容为预设名称

---@type LazySpec
local plugin_manager = require("config.plugin_manager")
local presets = require("config.presets")

-- 读取当前预设
local current_preset = presets.read_preset_file()
local preset_config = presets.apply(current_preset)

-- 构建插件列表
local plugins = plugin_manager.build_plugin_list(preset_config)

-- 显示当前配置信息
vim.defer_fn(function()
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
end, 500)

return plugins
