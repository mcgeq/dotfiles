-- ============================================================================
-- Python Development Environment: ty + ruff
-- ============================================================================
-- 使用 ty (快速类型检查器) 和 ruff (linter + formatter)
-- 替代 AstroCommunity python-ruff pack (使用 basedpyright)
--
-- 文档:
--   - ty:   https://docs.astral.sh/ty/
--   - ruff: https://docs.astral.sh/ruff/
--
-- 特性:
--   ✓ ty 类型检查（比 basedpyright 更快）
--   ✓ ruff linting 和 formatting
--   ✓ 自动使用项目虚拟环境（通过 uv run）
--   ✓ 智能 hover 管理（ty 提供类型信息，ruff 禁用 hover）
-- ============================================================================

-- ============================================================================
-- 配置常量
-- ============================================================================
local M = {}

-- ty LSP 配置
M.TY_CONFIG = {
  -- 命令配置：使用 uv run 自动处理虚拟环境
  cmd = { "uv", "run", "ty", "server" },
  
  -- 项目根目录标记文件
  root_markers = {
    "pyproject.toml",      -- Python 项目配置
    "setup.py",            -- 传统 Python 包
    "setup.cfg",           -- 配置文件
    "requirements.txt",    -- 依赖文件
    "pyrightconfig.json",  -- Pyright 配置（ty 兼容）
    ".git",                -- Git 仓库
  },
  
  -- ty 特定设置
  settings = {
    ty = {
      -- 可以在这里添加 ty 特定配置
      -- 例如：
      -- diagnosticMode = "workspace",  -- 诊断模式
      -- typeCheckingMode = "strict",   -- 类型检查严格度
    },
  },
}

-- ruff 格式化器配置
M.RUFF_FORMATTERS = {
  -- Import 排序
  organize_imports = {
    command = "ruff",
    args = { "check", "--select", "I", "--fix", "--stdin-filename", "$FILENAME" },
    stdin = true,
  },
  
  -- 代码格式化
  format = {
    command = "ruff",
    args = { "format", "--stdin-filename", "$FILENAME" },
    stdin = true,
  },
}

-- 需要移除的旧工具列表
M.DEPRECATED_TOOLS = {
  "black",        -- 被 ruff format 替代
  "isort",        -- 被 ruff 替代
  "pyright",      -- 被 ty 替代
  "basedpyright", -- 被 ty 替代
  "ruff-lsp",     -- 使用新的 ruff LSP
}

-- ============================================================================
-- 辅助函数
-- ============================================================================

--- 从工具列表中移除指定工具
---@param tools table 工具列表
---@param tool_name string 要移除的工具名称
local function remove_tool(tools, tool_name)
  for i, tool in ipairs(tools) do
    if tool == tool_name then
      table.remove(tools, i)
      return true
    end
  end
  return false
end

--- 批量移除废弃的工具
---@param tools table 工具列表
local function remove_deprecated_tools(tools)
  for _, tool_name in ipairs(M.DEPRECATED_TOOLS) do
    remove_tool(tools, tool_name)
  end
end

--- 检查当前预设是否需要 Python 工具
---@return boolean
local function should_install_python_tools()
  local preset = "fullstack"
  local ok, presets = pcall(require, "config.presets")
  
  if ok and presets and presets.read_preset_file then
    local success, result = pcall(presets.read_preset_file)
    if success and result then
      preset = result
    end
  end
  
  return preset == "backend" or preset == "fullstack"
end

-- ============================================================================
-- 插件配置
-- ============================================================================

---@type LazySpec
return {
  -- ==========================================================================
  -- Mason LSP 配置
  -- ==========================================================================
  {
    "williamboman/mason-lspconfig.nvim",
    opts = function(_, opts)
      -- 确保 ruff 被安装（ty 通过 pip/uv 安装，不在 Mason 中）
      opts.ensure_installed = opts.ensure_installed or {}
      vim.list_extend(opts.ensure_installed, { "ruff" })
    end,
  },

  -- ==========================================================================
  -- ty LSP 配置（使用 Neovim 0.11+ 新 API）
  -- ==========================================================================
  {
    "neovim/nvim-lspconfig",
    opts = function(_, opts)
      -- 创建 autocmd 在打开 Python 文件时配置 ty
      vim.api.nvim_create_autocmd("FileType", {
        pattern = "python",
        callback = function()
          -- 配置 ty LSP 服务器
          vim.lsp.config("ty", M.TY_CONFIG)
          
          -- 启用 ty LSP
          vim.lsp.enable("ty")
        end,
        desc = "Configure and enable ty LSP for Python",
      })
      
      return opts
    end,
  },

  -- ==========================================================================
  -- AstroLSP 配置
  -- ==========================================================================
  {
    "AstroNvim/astrolsp",
    ---@type AstroLSPOpts
    opts = {
      config = {
        -- ty LSP 服务器配置
        ty = {
          cmd = M.TY_CONFIG.cmd,
          filetypes = { "python" },
          root_dir = function(fname)
            local util = require("lspconfig.util")
            return util.root_pattern(unpack(M.TY_CONFIG.root_markers))(fname)
          end,
          single_file_support = true,
          settings = M.TY_CONFIG.settings,
          
          -- ty 特定的 on_attach 回调
          on_attach = function(client, bufnr)
            -- 可以在这里添加 ty 特定的键绑定或配置
            -- 例如：
            -- vim.keymap.set("n", "<leader>lt", function()
            --   vim.cmd("LspRestart ty")
            -- end, { buffer = bufnr, desc = "Restart ty LSP" })
          end,
        },
        
        -- ruff LSP 服务器配置
        ruff = {
          on_attach = function(client, bufnr)
            -- 禁用 ruff 的 hover 功能，避免与 ty 冲突
            -- ty 提供更好的类型信息
            client.server_capabilities.hoverProvider = false
            
            -- 可以添加 ruff 特定的键绑定
            -- 例如：
            -- vim.keymap.set("n", "<leader>lr", function()
            --   vim.cmd("LspRestart ruff")
            -- end, { buffer = bufnr, desc = "Restart ruff LSP" })
          end,
        },
        
        -- 显式禁用 basedpyright（如果存在）
        basedpyright = {
          enabled = false,
        },
        
        -- 显式禁用 pyright（如果存在）
        pyright = {
          enabled = false,
        },
      },
    },
  },

  -- ==========================================================================
  -- Mason 工具安装器配置
  -- ==========================================================================
  {
    "WhoIsSethDaniel/mason-tool-installer.nvim",
    optional = true,
    opts = function(_, opts)
      -- 只在 backend 或 fullstack 预设下配置 Python 工具
      if not should_install_python_tools() then
        return opts
      end

      opts.ensure_installed = opts.ensure_installed or {}
      
      -- 添加 ruff（ty 通过 pip/uv 安装，不在 Mason 中）
      vim.list_extend(opts.ensure_installed, { "ruff" })
      
      -- 移除所有废弃的 Python 工具
      remove_deprecated_tools(opts.ensure_installed)
      
      return opts
    end,
  },

  -- ==========================================================================
  -- Conform.nvim 格式化配置
  -- ==========================================================================
  {
    "stevearc/conform.nvim",
    optional = true,
    opts = function(_, opts)
      -- 配置 Python 文件使用 ruff 格式化
      opts.formatters_by_ft = opts.formatters_by_ft or {}
      opts.formatters_by_ft.python = { "ruff_organize_imports", "ruff_format" }
      
      -- 配置 ruff 格式化器
      opts.formatters = opts.formatters or {}
      opts.formatters.ruff_organize_imports = M.RUFF_FORMATTERS.organize_imports
      opts.formatters.ruff_format = M.RUFF_FORMATTERS.format
      
      return opts
    end,
  },
}
