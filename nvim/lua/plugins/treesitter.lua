-- Treesitter 配置
-- 语法高亮、代码解析、文本对象等
-- 文档: `:h nvim-treesitter`

---@type LazySpec
return {
  "nvim-treesitter/nvim-treesitter",
  opts = function(_, opts)
    -- ===== 自动安装的解析器 =====
    -- 注意：Windows 需要 C/C++ 编译器才能安装解析器
    -- 如果遇到编译错误，请安装 Visual Studio Build Tools 或 MinGW
    
    opts.ensure_installed = {
      -- ===== 核心 =====
      "lua",
      "vim",
      "vimdoc",
      "query",
      "regex",
      "comment",
      
      -- ===== 前端开发 =====
      "html",
      "css",
      "scss",
      "javascript",
      "typescript",
      "tsx",
      "vue",
      "json",
      "jsonc",
      
      -- ===== 后端开发 =====
      "rust",
      "go",
      "python",
      "c",
      "cpp",
      "zig",
      "cmake",
      
      -- ===== 脚本和配置 =====
      "bash",
      "yaml",
      "toml",
      "markdown",
      "markdown_inline",
      
      -- ===== DevOps =====
      "dockerfile",
      "git_config",
      "gitignore",
      "gitcommit",
      "git_rebase",
      
      -- ===== 其他可选 =====
      -- "java",
      -- "kotlin",
      -- "swift",
      -- "xml",
      -- "sql",
      -- "graphql",
    }
    
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
