-- Treesitter 配置
-- 语法高亮、代码解析、文本对象等
-- 文档: `:h nvim-treesitter`

---@type LazySpec
return {
  "nvim-treesitter/nvim-treesitter",
  opts = function(_, opts)
    -- ===== 自动安装的解析器（根据预设动态配置）=====
    -- 注意：Windows 需要 C/C++ 编译器才能安装解析器
    -- 如果遇到编译错误，请安装 Visual Studio Build Tools 或 MinGW
    
    -- 读取当前预设（带错误处理）
    local preset = "fullstack"
    local ok, presets = pcall(require, "config.presets")
    if ok and presets and presets.read_preset_file then
      local success, result = pcall(presets.read_preset_file)
      if success and result then
        preset = result
      end
    end
    
    -- 核心解析器（所有预设都需要）
    local core_parsers = {
      "lua",
      "vim",
      "vimdoc",
      "query",
      "regex",
      "comment",
      "markdown",
      "markdown_inline",
      "norg", -- neorg 笔记支持
    }
    
    -- 配置和脚本解析器（几乎所有预设都需要）
    local config_parsers = {
      "bash",
      "yaml",
      "toml",
      "json",
      "jsonc",
    }
    
    -- Git 相关解析器
    local git_parsers = {
      "git_config",
      "gitignore",
      "gitcommit",
      "git_rebase",
    }
    
    -- 前端解析器
    local frontend_parsers = {
      "html",
      "css",
      "scss",
      "javascript",
      "typescript",
      "tsx",
      "vue",
    }
    
    -- 后端解析器
    local backend_parsers = {
      "rust",
      "go",
      "python",
      "c",
      "cpp",
      "zig",
      "cmake",
    }
    
    -- DevOps 解析器
    local devops_parsers = {
      "dockerfile",
    }
    
    -- 根据预设组合解析器列表
    local parsers = vim.deepcopy(core_parsers)
    
    if preset == "minimal" then
      -- 最小化预设：只安装核心解析器
      vim.list_extend(parsers, config_parsers)
    elseif preset == "frontend" then
      -- 前端预设：核心 + 配置 + Git + 前端
      vim.list_extend(parsers, config_parsers)
      vim.list_extend(parsers, git_parsers)
      vim.list_extend(parsers, frontend_parsers)
      vim.list_extend(parsers, devops_parsers)
    elseif preset == "backend" then
      -- 后端预设：核心 + 配置 + Git + 后端
      vim.list_extend(parsers, config_parsers)
      vim.list_extend(parsers, git_parsers)
      vim.list_extend(parsers, backend_parsers)
      vim.list_extend(parsers, devops_parsers)
    elseif preset == "performance" then
      -- 性能预设：核心 + 配置
      vim.list_extend(parsers, config_parsers)
      vim.list_extend(parsers, git_parsers)
    else
      -- fullstack 或其他：安装所有
      vim.list_extend(parsers, config_parsers)
      vim.list_extend(parsers, git_parsers)
      vim.list_extend(parsers, frontend_parsers)
      vim.list_extend(parsers, backend_parsers)
      vim.list_extend(parsers, devops_parsers)
    end
    
    opts.ensure_installed = parsers
    
    -- ===== 功能配置 =====
    opts.highlight = {
      enable = true, -- 启用高亮
      additional_vim_regex_highlighting = false, -- 禁用 vim 正则高亮（性能优化）
    }
    
    opts.indent = {
      enable = true, -- 启用基于 treesitter 的缩进
    }
    
    opts.incremental_selection = {
      enable = true,
      keymaps = {
        init_selection = "<C-space>",
        node_incremental = "<C-space>",
        scope_incremental = false,
        node_decremental = "<bs>",
      },
    }
    
    -- 启用自动安装（现在有编译器了）
    opts.auto_install = true
    
    return opts
  end,
}
