-- Mason 配置
-- 自动安装和管理 LSP 服务器、格式化工具、调试器等
-- 文档: `:h mason.nvim`

---@type LazySpec
return {
  -- Mason 工具自动安装器
  {
    "WhoIsSethDaniel/mason-tool-installer.nvim",
    dependencies = {
      "williamboman/mason.nvim",
      "williamboman/mason-lspconfig.nvim",
    },
    event = "VeryLazy", -- 延迟加载，避免初始化顺序问题
    opts = function()
      -- 读取当前预设（带错误处理）
      local preset = "fullstack"
      local ok, presets = pcall(require, "config.presets")
      if ok and presets and presets.read_preset_file then
        local success, result = pcall(presets.read_preset_file)
        if success and result then
          preset = result
        end
      end
      
      -- ===== 工具分类定义 =====
      
      -- 核心工具（所有预设都需要）
      local core_tools = {
        "lua-language-server", -- Lua
        "stylua", -- Lua 格式化
        "bash-language-server", -- Bash
        "shfmt", -- Bash 格式化
        "shellcheck", -- Bash Linter
      }
      
      -- 配置文件 LSP（几乎所有预设都需要）
      local config_tools = {
        "yaml-language-server", -- YAML
        "json-lsp", -- JSON
        "taplo", -- TOML
      }
      
      -- 前端工具
      local frontend_tools = {
        "typescript-language-server", -- TypeScript/JavaScript
        "vue-language-server", -- Vue.js (Volar)
        "html-lsp", -- HTML
        "css-lsp", -- CSS/SCSS/Less
        "tailwindcss-language-server", -- Tailwind CSS
        "emmet-language-server", -- Emmet (HTML/CSS 补全)
        "biome", -- Biome (Linting + 格式化 + Import 排序)
      }
      
      -- 后端工具
      local backend_tools = {
        "rust-analyzer", -- Rust
        "gopls", -- Go
        "pyright", -- Python（类型检查）
        "ruff-lsp", -- Python（快速 linter + formatter）
        "clangd", -- C/C++
        "zls", -- Zig
        "cmake-language-server", -- CMake
        "clang-format", -- C/C++ 格式化
      }
      
      -- 调试器
      local debuggers = {
        "debugpy", -- Python
        "codelldb", -- C/C++/Rust
        "delve", -- Go
      }
      
      -- DevOps 工具
      local devops_tools = {
        "dockerfile-language-server", -- Dockerfile
      }
      
      -- ===== 根据预设组合工具列表 =====
      local tools = vim.deepcopy(core_tools)
      
      if preset == "minimal" then
        -- 最小化预设：只安装核心工具
        vim.list_extend(tools, config_tools)
      elseif preset == "frontend" then
        -- 前端预设：核心 + 配置 + 前端 + DevOps
        vim.list_extend(tools, config_tools)
        vim.list_extend(tools, frontend_tools)
        vim.list_extend(tools, devops_tools)
        -- 前端通常用浏览器调试，不安装 debuggers
      elseif preset == "backend" then
        -- 后端预设：核心 + 配置 + 后端 + DevOps + 调试器
        vim.list_extend(tools, config_tools)
        vim.list_extend(tools, backend_tools)
        vim.list_extend(tools, devops_tools)
        vim.list_extend(tools, debuggers)
      elseif preset == "performance" then
        -- 性能预设：核心 + 配置（减少工具数量）
        vim.list_extend(tools, config_tools)
      else
        -- fullstack 或其他：安装所有
        vim.list_extend(tools, config_tools)
        vim.list_extend(tools, frontend_tools)
        vim.list_extend(tools, backend_tools)
        vim.list_extend(tools, devops_tools)
        vim.list_extend(tools, debuggers)
      end
      
      return {
        ensure_installed = tools,
        auto_update = false, -- 建议手动更新
        run_on_start = false, -- 禁用自动运行，避免初始化顺序问题
      }
    end,
    config = function(_, opts)
      local installer_setup = false
      
      -- 延迟初始化，确保 mason-lspconfig 已完全加载
      vim.defer_fn(function()
        local ok, installer = pcall(require, "mason-tool-installer")
        if ok then
          installer.setup(opts)
          installer_setup = true
          
          -- 提示用户可以手动安装
          vim.notify(
            "Mason Tool Installer ready. Use :MasonToolsInstall to install tools.",
            vim.log.levels.INFO
          )
        else
          vim.notify("Failed to load mason-tool-installer", vim.log.levels.WARN)
        end
      end, 200) -- 延迟 200ms 确保依赖已加载
      
      -- 创建用户命令手动触发安装
      vim.api.nvim_create_user_command("MasonToolsInstallNow", function()
        if installer_setup then
          require("mason-tool-installer").check_install(true)
        else
          vim.notify("Mason Tool Installer not ready yet, please wait...", vim.log.levels.WARN)
        end
      end, { desc = "Install Mason tools now" })
    end,
  },
}
