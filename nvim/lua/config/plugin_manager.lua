-- 插件分组管理器
-- 将 community.lua 中的插件按用途分组，提升可维护性

local M = {}

-- 语言包配置
M.language_packs = {
  -- 前端开发
  frontend = {
    { import = "astrocommunity.pack.typescript-all-in-one" },
    { import = "astrocommunity.pack.vue" },
    { import = "astrocommunity.pack.biome" },
    { import = "astrocommunity.pack.html-css" },
    { import = "astrocommunity.pack.tailwindcss" },
  },
  
  -- 后端开发
  backend = {
    { import = "astrocommunity.pack.rust" },
    { import = "astrocommunity.pack.go" },
    { import = "astrocommunity.pack.python-ruff" },
    { import = "astrocommunity.pack.cpp" },
    { import = "astrocommunity.pack.zig" },
  },
  
  -- 配置和数据
  config = {
    { import = "astrocommunity.pack.json" },
    { import = "astrocommunity.pack.yaml" },
    { import = "astrocommunity.pack.toml" },
  },
  
  -- 脚本工具
  scripting = {
    { import = "astrocommunity.pack.lua" },
    { import = "astrocommunity.pack.bash" },
  },
  
  -- DevOps
  devops = {
    { import = "astrocommunity.pack.docker" },
    { import = "astrocommunity.pack.cmake" },
  },
  
  -- 文档
  docs = {
    { import = "astrocommunity.pack.markdown" },
  },
  
  -- 版本控制
  vcs = {
    { import = "astrocommunity.pack.jj" },
  },
}

-- 编辑器增强配置
M.editor_features = {
  completion = {
    { import = "astrocommunity.completion.blink-cmp" },
    { import = "astrocommunity.completion.blink-cmp-emoji" },
    { import = "astrocommunity.completion.blink-cmp-git" },
  },
  
  editing = {
    { import = "astrocommunity.editing-support.bigfile-nvim" },
    { import = "astrocommunity.editing-support.conform-nvim" },
    { import = "astrocommunity.editing-support.nvim-treesitter-context" },
    { import = "astrocommunity.editing-support.todo-comments-nvim" },
    { import = "astrocommunity.editing-support.zen-mode-nvim" },
    { import = "astrocommunity.editing-support.yanky-nvim" },
    { import = "astrocommunity.editing-support.neogen" },
    { import = "astrocommunity.editing-support.refactoring-nvim" },
  },
  
  motion = {
    { import = "astrocommunity.motion.flash-nvim" },
    { import = "astrocommunity.motion.mini-surround" },
    { import = "astrocommunity.motion.mini-ai" },
    { import = "astrocommunity.motion.nvim-spider" },
    { import = "astrocommunity.motion.harpoon" },
  },
  
  git = {
    { import = "astrocommunity.git.blame-nvim" },
    { import = "astrocommunity.git.gist-nvim" },
    { import = "astrocommunity.git.diffview-nvim" },
    { import = "astrocommunity.git.gitlinker-nvim" },
  },
  
  ui = {
    { import = "astrocommunity.pack.rainbow-delimiter-indent-blankline" },
    { import = "astrocommunity.utility.noice-nvim" },
    { import = "astrocommunity.color.ccc-nvim" },
    { import = "astrocommunity.split-and-window.edgy-nvim" },
    { import = "astrocommunity.split-and-window.colorful-winsep-nvim" },
    { import = "astrocommunity.split-and-window.windows-nvim" },
  },
}

-- 工作流工具
M.workflow_tools = {
  search = {
    { import = "astrocommunity.fuzzy-finder.snacks-picker" },
    { import = "astrocommunity.search.nvim-spectre" },
  },
  
  diagnostics = {
    { import = "astrocommunity.diagnostics.trouble-nvim" },
  },
  
  debugging = {
    { import = "astrocommunity.debugging.nvim-dap-virtual-text" },
    { import = "astrocommunity.debugging.persistent-breakpoints-nvim" },
  },
  
  testing = {
    { import = "astrocommunity.test.neotest" },
  },
  
  productivity = {
    { import = "astrocommunity.workflow.hardtime-nvim" },
    { import = "astrocommunity.workflow.precognition-nvim" },
    { import = "astrocommunity.utility.neodim" },
    { import = "astrocommunity.utility.nvim-toggler" },
  },
  
  project = {
    { import = "astrocommunity.project.project-nvim" },
  },
  
  explorer = {
    { import = "astrocommunity.file-explorer.mini-files" },
  },
  
  lsp = {
    { import = "astrocommunity.lsp.actions-preview-nvim" },
  },
  
  comment = {
    { import = "astrocommunity.comment.ts-comments-nvim" },
  },
  
  markdown = {
    { import = "astrocommunity.markdown-and-latex.markdown-preview-nvim" },
  },
  
  note_taking = {
    { import = "astrocommunity.note-taking.neorg" },
  },
}

--- 构建完整的插件列表
---@param config table? 自定义配置（可选）
---@return table plugins 插件列表
function M.build_plugin_list(config)
  config = config or {}
  
  local plugins = { "AstroNvim/astrocommunity" }
  
  -- 添加所有启用的语言包
  for category, packs in pairs(M.language_packs) do
    if config.disable_languages and config.disable_languages[category] then
      -- 跳过被禁用的语言分类
    else
      vim.list_extend(plugins, packs)
    end
  end
  
  -- 添加所有启用的编辑器功能
  for category, features in pairs(M.editor_features) do
    if config.disable_features and config.disable_features[category] then
      -- 跳过被禁用的功能分类
    else
      vim.list_extend(plugins, features)
    end
  end
  
  -- 添加所有启用的工作流工具
  for category, tools in pairs(M.workflow_tools) do
    if config.disable_tools and config.disable_tools[category] then
      -- 跳过被禁用的工具分类
    else
      vim.list_extend(plugins, tools)
    end
  end
  
  return plugins
end

--- 获取插件统计信息
---@return table stats 统计信息
function M.get_stats()
  local function count_plugins(t)
    local count = 0
    for _, v in pairs(t) do
      count = count + #v
    end
    return count
  end
  
  return {
    language_packs = count_plugins(M.language_packs),
    editor_features = count_plugins(M.editor_features),
    workflow_tools = count_plugins(M.workflow_tools),
  }
end

return M
