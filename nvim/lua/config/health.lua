-- 配置健康检查
-- 运行 :checkhealth config 来检查配置状态

local M = {}

--- 健康检查主函数
function M.check()
  -- 兼容 Neovim 0.9+ 和 0.10+
  local health = vim.health or require("health")
  
  health.start("Custom Configuration")
  
  -- ===== 检查核心配置模块 =====
  health.info("Checking core configuration modules...")
  
  local core_modules = {
    { name = "config.constants", desc = "Global constants" },
    { name = "config.loader", desc = "Configuration loader" },
    { name = "config.auto_update_timestamp", desc = "Timestamp updater" },
    { name = "config.snippets", desc = "Snippets configuration" },
  }
  
  for _, module in ipairs(core_modules) do
    local ok, result = pcall(require, module.name)
    if ok then
      health.ok(string.format("%s loaded successfully", module.desc))
    else
      health.error(
        string.format("Failed to load %s", module.desc),
        { string.format("Error: %s", result) }
      )
    end
  end
  
  -- ===== 检查常量配置 =====
  health.info("Checking constants configuration...")
  
  local ok, constants = pcall(require, "config.constants")
  if ok then
    if constants.user and constants.user.name and constants.user.email then
      health.ok(string.format("User info configured: %s <%s>", constants.user.name, constants.user.email))
    else
      health.warn("User info not properly configured", { "Edit lua/config/constants.lua to set your information" })
    end
    
    if constants.comment_styles and vim.tbl_count(constants.comment_styles) > 0 then
      health.ok(string.format("Comment styles defined: %d types", vim.tbl_count(constants.comment_styles)))
    else
      health.error("Comment styles not defined")
    end
  end
  
  -- ===== 检查必需插件 =====
  health.start("Required Plugins")
  
  local required_plugins = {
    { name = "astrocore", desc = "AstroCore" },
    { name = "astrolsp", desc = "AstroLSP" },
    { name = "astroui", desc = "AstroUI" },
    { name = "lazy", desc = "Lazy.nvim" },
  }
  
  for _, plugin in ipairs(required_plugins) do
    local ok = pcall(require, plugin.name)
    if ok then
      health.ok(string.format("%s is available", plugin.desc))
    else
      health.error(
        string.format("%s not found", plugin.desc),
        { "Run :Lazy sync to install missing plugins" }
      )
    end
  end
  
  -- ===== 检查 LSP 配置 =====
  health.start("LSP Configuration")
  
  local lsp_clients = vim.lsp.get_clients or vim.lsp.get_active_clients
  local clients = lsp_clients()
  
  if #clients > 0 then
    health.ok(string.format("%d LSP client(s) active", #clients))
    for _, client in ipairs(clients) do
      health.info(string.format("  • %s", client.name))
    end
  else
    health.info("No LSP clients active (open a file to check)")
  end
  
  -- ===== 检查 Mason 工具 =====
  health.start("Mason Tools")
  
  local mason_ok, mason_registry = pcall(require, "mason-registry")
  if mason_ok then
    local installed = mason_registry.get_installed_package_names()
    if #installed > 0 then
      health.ok(string.format("%d tool(s) installed via Mason", #installed))
    else
      health.warn("No tools installed via Mason", { "Run :Mason to install tools" })
    end
  else
    health.info("Mason not available (this is optional)")
  end
  
  -- ===== 检查 Treesitter =====
  health.start("Treesitter")
  
  local ts_ok, ts_parsers = pcall(require, "nvim-treesitter.parsers")
  if ts_ok then
    local available_parsers = ts_parsers.available_parsers()
    local installed_parsers = {}
    for _, parser in ipairs(available_parsers) do
      if ts_parsers.has_parser(parser) then
        table.insert(installed_parsers, parser)
      end
    end
    
    if #installed_parsers > 0 then
      health.ok(string.format("%d parser(s) installed", #installed_parsers))
    else
      health.warn("No parsers installed", { "Run :TSInstall <language> to install parsers" })
    end
  else
    health.info("Treesitter not available")
  end
  
  -- ===== 检查性能设置 =====
  health.start("Performance Settings")
  
  local updatetime = vim.opt.updatetime:get()
  if updatetime <= 300 then
    health.ok(string.format("updatetime set to %dms (recommended: ≤300ms)", updatetime))
  else
    health.warn(
      string.format("updatetime is %dms", updatetime),
      { "Consider setting it to 200-300ms for better responsiveness" }
    )
  end
  
  local timeoutlen = vim.opt.timeoutlen:get()
  if timeoutlen <= 500 then
    health.ok(string.format("timeoutlen set to %dms", timeoutlen))
  else
    health.warn(
      string.format("timeoutlen is %dms", timeoutlen),
      { "Consider setting it to 300-500ms for faster key mappings" }
    )
  end
  
  -- ===== 检查文件配置 =====
  health.start("File Settings")
  
  if vim.opt.undofile:get() then
    health.ok("Persistent undo enabled")
  else
    health.info("Persistent undo disabled")
  end
  
  if not vim.opt.swapfile:get() then
    health.ok("Swap files disabled (recommended)")
  else
    health.info("Swap files enabled")
  end
  
  -- ===== 总结 =====
  health.start("Summary")
  health.info("Run :Lazy profile to check plugin loading times")
  health.info("Run :Mason to manage LSP servers and tools")
  health.info("Run :TSUpdate to update Treesitter parsers")
end

return M
