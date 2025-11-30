-- Mason 配置
-- 自动安装和管理 LSP 服务器、格式化工具、调试器等
-- 文档: `:h mason.nvim`

---@type LazySpec
return {
  -- Mason 工具自动安装器
  {
    "WhoIsSethDaniel/mason-tool-installer.nvim",
    opts = {
      -- ===== 自动安装的工具列表 =====
      -- 使用 `:Mason` 查看可用工具名称
      ensure_installed = {
        -- ============================================
        -- === 前端开发 ===
        -- ============================================
        "typescript-language-server", -- TypeScript/JavaScript
        "vue-language-server", -- Vue.js (Volar)
        "html-lsp", -- HTML
        "css-lsp", -- CSS/SCSS/Less
        "tailwindcss-language-server", -- Tailwind CSS
        "emmet-language-server", -- Emmet (HTML/CSS 补全)
        
        -- === 前端格式化和 Linting ===
        "biome", -- Biome (Linting + 格式化 + Import 排序，支持 Vue)
        
        -- ============================================
        -- === 后端开发 ===
        -- ============================================
        "rust-analyzer", -- Rust
        "gopls", -- Go
        "pyright", -- Python（类型检查）
        "ruff-lsp", -- Python（快速 linter + formatter）
        "clangd", -- C/C++
        "zls", -- Zig
        "cmake-language-server", -- CMake
        
        -- === 后端格式化工具 ===
        "clang-format", -- C/C++
        -- "gofmt", -- Go（gopls 自带，不需要单独安装）
        
        -- === Python 工具（使用 Ruff 统一处理）===
        -- Ruff 已经包含：Linting + 格式化 + import 排序
        -- 不再需要：black, isort, pylint
        -- "black", -- Python 格式化（Ruff 已包含）
        -- "isort", -- Python imports 排序（Ruff 已包含）
        -- "pylint", -- Python Linter（Ruff 更快且兼容）
        
        -- ============================================
        -- === 调试器（Debuggers）===
        -- ============================================
        "debugpy", -- Python
        "codelldb", -- C/C++/Rust
        "delve", -- Go
        
        -- ============================================
        -- === 脚本和配置 ===
        -- ============================================
        "lua-language-server", -- Lua
        "bash-language-server", -- Bash
        "stylua", -- Lua 格式化
        "shfmt", -- Bash 格式化
        "shellcheck", -- Bash Linter
        
        -- ============================================
        -- === 配置文件 LSP ===
        -- ============================================
        "yaml-language-server", -- YAML
        "json-lsp", -- JSON
        "taplo", -- TOML
        
        -- ============================================
        -- === DevOps ===
        -- ============================================
        "dockerfile-language-server", -- Dockerfile
        -- "docker-compose-language-server", -- ⚠️ Mason 中不存在此包
        
        -- ============================================
        -- === 可选工具（按需启用）===
        -- ============================================
        -- "tree-sitter-cli", -- Tree-sitter CLI（通常不需要手动安装）
        -- "marksman", -- Markdown LSP
        -- "vale", -- Markdown/文档 linter
      },
      
      -- 自动更新工具
      auto_update = false, -- 是否自动更新工具（建议手动更新）
      
      -- 安装失败时的行为
      run_on_start = true, -- 启动时检查并安装缺失的工具
    },
  },
}
