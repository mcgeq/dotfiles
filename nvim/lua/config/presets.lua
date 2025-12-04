-- 配置预设系统
-- 根据不同使用场景快速切换配置

local M = {}

-- 预设配置：最小化（快速启动）
M.minimal = {
  disable_languages = {
    backend = true, -- 禁用后端语言
    devops = true,
  },
  disable_features = {
    git = true, -- 禁用 git 工具
    ui = true, -- 禁用 UI 增强
  },
  disable_tools = {
    debugging = true,
    testing = true,
    productivity = true,
  },
}

-- 预设配置：前端开发
M.frontend = {
  disable_languages = {
    backend = true, -- 只保留前端相关
  },
  disable_tools = {
    debugging = true, -- 前端通常用浏览器调试
  },
}

-- 预设配置：后端开发
M.backend = {
  disable_languages = {
    frontend = true, -- 只保留后端相关
  },
}

-- 预设配置：全栈开发（默认）
M.fullstack = {
  -- 启用所有功能
}

-- 预设配置：性能模式（减少插件数量）
M.performance = {
  disable_features = {
    ui = true,
  },
  disable_tools = {
    productivity = true,
    markdown = true,
  },
}

--- 获取当前激活的预设
---@return string preset 预设名称
function M.get_active_preset()
  -- 从环境变量或配置文件读取
  return vim.env.NVIM_PRESET or "fullstack"
end

--- 应用预设配置
---@param preset_name string 预设名称
---@return table config 预设配置
function M.apply(preset_name)
  local preset = M[preset_name]
  if not preset then
    vim.notify(
      string.format("Preset '%s' not found, using 'fullstack'", preset_name),
      vim.log.levels.WARN
    )
    return M.fullstack
  end
  return preset
end

--- 列出所有可用预设
---@return table presets 预设列表
function M.list_presets()
  return {
    { name = "minimal", desc = "最小化配置（快速启动）" },
    { name = "frontend", desc = "前端开发（JS/TS/Vue）" },
    { name = "backend", desc = "后端开发（Rust/Go/Python）" },
    { name = "fullstack", desc = "全栈开发（所有功能）" },
    { name = "performance", desc = "性能模式（减少插件）" },
  }
end

--- 切换预设（需要重启 Neovim）
---@param preset_name string 预设名称
function M.switch_to(preset_name)
  if not M[preset_name] then
    vim.notify("Invalid preset: " .. preset_name, vim.log.levels.ERROR)
    return
  end
  
  -- 保存到配置文件
  local preset_file = vim.fn.stdpath("config") .. "/.preset"
  local file = io.open(preset_file, "w")
  if file then
    file:write(preset_name)
    file:close()
    vim.notify(
      string.format("Switched to preset '%s'. Please restart Neovim.", preset_name),
      vim.log.levels.INFO
    )
  else
    vim.notify("Failed to save preset", vim.log.levels.ERROR)
  end
end

--- 从文件读取预设
---@return string preset 预设名称
function M.read_preset_file()
  local preset_file = vim.fn.stdpath("config") .. "/.preset"
  local file = io.open(preset_file, "r")
  if file then
    local preset = file:read("*l")
    file:close()
    return preset or "fullstack"
  end
  return "fullstack"
end

return M
