-- 前端开发增强配置
-- 提供更好的前端开发体验

-- 读取预设，仅在 frontend 或 fullstack 预设时加载（带错误处理）
local preset = "fullstack"
local ok, presets = pcall(require, "config.presets")
if ok and presets and presets.read_preset_file then
  local success, result = pcall(presets.read_preset_file)
  if success and result then
    preset = result
  end
end
local should_load = preset == "frontend" or preset == "fullstack"

if not should_load then
  return {}
end

---@type LazySpec
return {
  -- ===== TypeScript 工具增强 =====
  {
    "pmizio/typescript-tools.nvim",
    ft = { "typescript", "typescriptreact", "javascript", "javascriptreact" },
    dependencies = { "nvim-lua/plenary.nvim", "neovim/nvim-lspconfig" },
    opts = {
      settings = {
        separate_diagnostic_server = true,
        publish_diagnostic_on = "insert_leave",
        expose_as_code_action = "all",
        tsserver_file_preferences = {
          includeInlayParameterNameHints = "all",
          includeInlayParameterNameHintsWhenArgumentMatchesName = false,
          includeInlayFunctionParameterTypeHints = true,
          includeInlayVariableTypeHints = true,
          includeInlayVariableTypeHintsWhenTypeMatchesName = false,
          includeInlayPropertyDeclarationTypeHints = true,
          includeInlayFunctionLikeReturnTypeHints = true,
          includeInlayEnumMemberValueHints = true,
        },
        tsserver_format_options = {
          allowIncompleteCompletions = false,
          allowRenameOfImportPath = false,
        },
      },
    },
  },

  -- ===== Vue 开发 =====
  -- 注意：Vue 支持已由 astrocommunity.pack.vue 提供（包含 Volar LSP）

  -- ===== Tailwind CSS 智能提示 =====
  {
    "roobert/tailwindcss-colorizer-cmp.nvim",
    ft = { "html", "css", "javascript", "typescript", "javascriptreact", "typescriptreact", "vue", "svelte" },
    config = function()
      require("tailwindcss-colorizer-cmp").setup({
        color_square_width = 2,
      })
    end,
  },

  -- ===== CSS 颜色预览 =====
  {
    "NvChad/nvim-colorizer.lua",
    ft = { "css", "scss", "sass", "less", "html", "javascript", "typescript", "vue", "svelte" },
    opts = {
      filetypes = { "*" },
      user_default_options = {
        RGB = true,
        RRGGBB = true,
        names = true,
        RRGGBBAA = true,
        AARRGGBB = true,
        rgb_fn = true,
        hsl_fn = true,
        css = true,
        css_fn = true,
        mode = "background",
        tailwind = true,
        sass = { enable = true, parsers = { "css" } },
        virtualtext = "■",
      },
    },
  },

  -- ===== 自动重命名 HTML/JSX 标签 =====
  {
    "windwp/nvim-ts-autotag",
    ft = {
      "html",
      "javascript",
      "typescript",
      "javascriptreact",
      "typescriptreact",
      "svelte",
      "vue",
      "tsx",
      "jsx",
      "xml",
      "markdown",
    },
    opts = {
      opts = {
        enable_close = true,
        enable_rename = true,
        enable_close_on_slash = false,
      },
    },
  },

  -- ===== JSON Schema 支持 =====
  -- 注意：SchemaStore 已由 astrocommunity.pack.json 提供

  -- ===== Package.json 版本显示 =====
  {
    "vuki656/package-info.nvim",
    ft = "json",
    dependencies = { "MunifTanjim/nui.nvim" },
    opts = {
      highlights = {
        up_to_date = { fg = "#3C4048" },
        outdated = { fg = "#d19a66" },
      },
      icons = {
        enable = true,
        style = {
          up_to_date = "|  ",
          outdated = "|  ",
        },
      },
      autostart = true,
      hide_up_to_date = false,
      hide_unstable_versions = false,
    },
    keys = {
      { "<leader>ns", "<cmd>lua require('package-info').show()<cr>", desc = "Show package versions" },
      { "<leader>nc", "<cmd>lua require('package-info').hide()<cr>", desc = "Hide package versions" },
      { "<leader>nu", "<cmd>lua require('package-info').update()<cr>", desc = "Update package" },
      { "<leader>nd", "<cmd>lua require('package-info').delete()<cr>", desc = "Delete package" },
      { "<leader>ni", "<cmd>lua require('package-info').install()<cr>", desc = "Install package" },
      { "<leader>np", "<cmd>lua require('package-info').change_version()<cr>", desc = "Change package version" },
    },
  },

  -- ===== REST API 测试 =====
  {
    "rest-nvim/rest.nvim",
    ft = "http",
    dependencies = { "nvim-lua/plenary.nvim" },
    keys = {
      { "<leader>rr", "<Plug>RestNvim", desc = "Run request under cursor" },
      { "<leader>rp", "<Plug>RestNvimPreview", desc = "Preview request" },
      { "<leader>rl", "<Plug>RestNvimLast", desc = "Re-run last request" },
    },
    opts = {
      result_split_horizontal = false,
      result_split_in_place = false,
      skip_ssl_verification = false,
      encode_url = true,
      highlight = {
        enabled = true,
        timeout = 150,
      },
      result = {
        show_url = true,
        show_http_info = true,
        show_headers = true,
        formatters = {
          json = "jq",
          html = function(body)
            return vim.fn.system({ "tidy", "-i", "-q", "-" }, body)
          end,
        },
      },
      jump_to_request = false,
      env_file = ".env",
      custom_dynamic_variables = {},
      yank_dry_run = true,
    },
  },

  -- ===== Live Server（浏览器实时预览）=====
  {
    "barrett-ruth/live-server.nvim",
    ft = { "html", "css", "javascript" },
    build = "npm install -g live-server",
    cmd = { "LiveServerStart", "LiveServerStop" },
    keys = {
      { "<leader>ls", "<cmd>LiveServerStart<cr>", desc = "Live Server Start" },
      { "<leader>lx", "<cmd>LiveServerStop<cr>", desc = "Live Server Stop" },
    },
    config = true,
  },

  -- ===== Linting & Formatting =====
  -- 注意：前端统一使用 Biome (配置在 conform.lua 和 astrolsp.lua)
  -- Biome 提供格式化、Linting、Import 排序，支持：
  --  - JavaScript/TypeScript
  --  - JSX/TSX
  --  - JSON/JSONC
  --  - Vue (部分支持)
  -- 配置文件：conform.lua (使用 --unsafe 进行代码修复)
}
