-- 用户命令
-- 便捷访问配置管理功能

local M = {}

--- 注册所有用户命令
function M.setup()
  -- 预设管理命令
  vim.api.nvim_create_user_command("PresetList", function()
    local presets = require("config.presets").list_presets()
    local current = require("config.presets").read_preset_file()
    
    print("\n=== Available Presets ===\n")
    for _, preset in ipairs(presets) do
      local marker = preset.name == current and "→" or " "
      print(string.format("%s %-15s - %s", marker, preset.name, preset.desc))
    end
    print(string.format("\nCurrent: %s", current))
  end, { desc = "List available presets" })
  
  vim.api.nvim_create_user_command("PresetSwitch", function(opts)
    local preset_name = opts.args
    require("config.presets").switch_to(preset_name)
  end, {
    nargs = 1,
    complete = function()
      local presets = require("config.presets").list_presets()
      return vim.tbl_map(function(p) return p.name end, presets)
    end,
    desc = "Switch to a different preset",
  })
  
  -- 键位映射文档
  vim.api.nvim_create_user_command("KeymapDocs", function()
    require("config.keymaps").print_docs()
  end, { desc = "Show keymaps documentation" })
  
  -- 配置验证
  vim.api.nvim_create_user_command("ConfigValidate", function()
    local validator = require("config.validator")
    local report = validator.run_all_checks()
    validator.display_report(report)
  end, { desc = "Validate configuration" })
  
  -- 插件统计
  vim.api.nvim_create_user_command("PluginStats", function()
    local plugin_manager = require("config.plugin_manager")
    local stats = plugin_manager.get_stats()
    
    print("\n=== Plugin Statistics ===\n")
    print(string.format("Language Packs: %d", stats.language_packs))
    print(string.format("Editor Features: %d", stats.editor_features))
    print(string.format("Workflow Tools: %d", stats.workflow_tools))
    print(string.format("Total: %d", stats.language_packs + stats.editor_features + stats.workflow_tools))
  end, { desc = "Show plugin statistics" })
  
  -- 配置信息
  vim.api.nvim_create_user_command("ConfigInfo", function()
    local preset = require("config.presets").read_preset_file()
    local stats = require("config.plugin_manager").get_stats()
    
    local info = {
      "=== Neovim Configuration Info ===",
      "",
      string.format("Preset: %s", preset),
      string.format("Config Path: %s", vim.fn.stdpath("config")),
      string.format("Data Path: %s", vim.fn.stdpath("data")),
      string.format("Neovim Version: %s", vim.version().major .. "." .. vim.version().minor),
      "",
      "Plugin Statistics:",
      string.format("  Language Packs: %d", stats.language_packs),
      string.format("  Editor Features: %d", stats.editor_features),
      string.format("  Workflow Tools: %d", stats.workflow_tools),
    }
    
    print(table.concat(info, "\n"))
  end, { desc = "Show configuration information" })
end

return M
