-- 后端开发增强配置
-- 提供更好的后端开发体验
-- 支持：Go, Python

-- 注意：以下语言支持已由 AstroNvim pack 提供
--  - astrocommunity.pack.cpp (包含 clangd LSP)
--  - astrocommunity.pack.zig (包含 zls LSP + zig.vim)
--  - astrocommunity.pack.rust (包含 rust-analyzer + crates.nvim + rustaceanvim)

-- 读取预设，仅在 backend 或 fullstack 预设时加载（带错误处理）
local preset = "fullstack"
local ok, presets = pcall(require, "config.presets")
if ok and presets and presets.read_preset_file then
  local success, result = pcall(presets.read_preset_file)
  if success and result then
    preset = result
  end
end
local should_load = preset == "backend" or preset == "fullstack"

if not should_load then
  return {}
end

---@type LazySpec
return {

  -- ===== Go 工具增强 =====
  {
    "ray-x/go.nvim",
    ft = { "go", "gomod" },
    dependencies = {
      "ray-x/guihua.lua",
      "neovim/nvim-lspconfig",
      "nvim-treesitter/nvim-treesitter",
    },
    opts = {
      disable_defaults = false,
      go = "go",
      goimports = "gopls",
      fillstruct = "gopls",
      gofmt = "gofumpt",
      max_line_len = 120,
      tag_transform = false,
      test_template = "",
      test_template_dir = "",
      comment_placeholder = "",
      lsp_cfg = true,
      lsp_gofumpt = true,
      lsp_on_attach = true,
      dap_debug = true,
      dap_debug_gui = true,
      dap_debug_keymap = true,
      dap_vt = true,
      build_tags = "",
      textobjects = true,
      test_runner = "go",
      run_in_floaterm = false,
      luasnip = true,
    },
    config = function(_, opts)
      require("go").setup(opts)
      
      -- 自动格式化和 import
      local format_sync_grp = vim.api.nvim_create_augroup("GoFormat", {})
      vim.api.nvim_create_autocmd("BufWritePre", {
        pattern = "*.go",
        callback = function()
          require("go.format").goimports()
        end,
        group = format_sync_grp,
      })
    end,
    keys = {
      { "<leader>gfs", "<cmd>GoFillStruct<cr>", desc = "Fill struct" },
      { "<leader>gie", "<cmd>GoIfErr<cr>", desc = "Add if err" },
      { "<leader>gat", "<cmd>GoAddTag<cr>", desc = "Add tags to struct" },
      { "<leader>grt", "<cmd>GoRmTag<cr>", desc = "Remove tags from struct" },
      { "<leader>gim", "<cmd>GoImpl<cr>", desc = "Generate interface implementation" },
      { "<leader>gtf", "<cmd>GoTestFunc<cr>", desc = "Test function" },
      { "<leader>gta", "<cmd>GoTest<cr>", desc = "Test all" },
      { "<leader>gtc", "<cmd>GoCoverage<cr>", desc = "Test coverage" },
    },
  },

  -- ===== Python 工具增强 =====
  -- 注意：Python 虚拟环境自动检测已移至 python-venv-auto.lua
  -- 使用简单的自动检测脚本，不依赖外部插件
  -- 自动检测并激活项目目录下的 .venv 或 venv


  -- ===== 数据库客户端 =====
  {
    "kristijanhusak/vim-dadbod-ui",
    dependencies = {
      { "tpope/vim-dadbod", lazy = true },
      { "kristijanhusak/vim-dadbod-completion", ft = { "sql", "mysql", "plsql" }, lazy = true },
    },
    cmd = {
      "DBUI",
      "DBUIToggle",
      "DBUIAddConnection",
      "DBUIFindBuffer",
    },
    init = function()
      vim.g.db_ui_use_nerd_fonts = 1
      vim.g.db_ui_win_position = "right"
      vim.g.db_ui_winwidth = 40
      vim.g.db_ui_show_database_icon = 1
      vim.g.db_ui_force_echo_notifications = 1
      vim.g.db_ui_save_location = vim.fn.stdpath("data") .. "/db_ui"
    end,
    keys = {
      { "<leader>db", "<cmd>DBUIToggle<cr>", desc = "Toggle Database UI" },
      { "<leader>df", "<cmd>DBUIFindBuffer<cr>", desc = "Find DB buffer" },
      { "<leader>dr", "<cmd>DBUIRenameBuffer<cr>", desc = "Rename DB buffer" },
      { "<leader>dq", "<cmd>DBUILastQueryInfo<cr>", desc = "Last query info" },
    },
  },

  -- ===== Docker 集成 =====
  {
    "mgierada/lazydocker.nvim",
    dependencies = { "akinsho/toggleterm.nvim" },
    cmd = { "LazyDocker" },
    keys = {
      { "<leader>ld", "<cmd>LazyDocker<cr>", desc = "LazyDocker" },
    },
    opts = {},
  },

  -- ===== HTTP 客户端（替代 Postman）=====
  {
    "jellydn/hurl.nvim",
    dependencies = {
      "MunifTanjim/nui.nvim",
      "nvim-lua/plenary.nvim",
      "nvim-treesitter/nvim-treesitter",
    },
    ft = "hurl",
    opts = {
      debug = false,
      show_notification = false,
      mode = "split",
      formatters = {
        json = { "jq" },
        html = {
          "prettier",
          "--parser",
          "html",
        },
      },
    },
    keys = {
      { "<leader>HA", "<cmd>HurlRunner<cr>", desc = "Run All requests" },
      { "<leader>Ha", "<cmd>HurlRunnerAt<cr>", desc = "Run Api request" },
      { "<leader>Hte", "<cmd>HurlRunnerToEntry<cr>", desc = "Run Api request to entry" },
      { "<leader>Htm", "<cmd>HurlToggleMode<cr>", desc = "Toggle split/popup" },
      { "<leader>Hv", "<cmd>HurlVerbose<cr>", desc = "Run request in verbose mode" },
      { "<leader>Hh", ":HurlRunner<cr>", desc = "Run request", mode = "v" },
    },
  },

  -- ===== Git Worktree 管理 =====
  {
    "ThePrimeagen/git-worktree.nvim",
    dependencies = { "nvim-telescope/telescope.nvim" },
    keys = {
      {
        "<leader>gwc",
        function()
          require("telescope").extensions.git_worktree.create_git_worktree()
        end,
        desc = "Create Worktree",
      },
      {
        "<leader>gws",
        function()
          require("telescope").extensions.git_worktree.git_worktrees()
        end,
        desc = "Switch Worktree",
      },
    },
    config = function()
      require("git-worktree").setup({})
      require("telescope").load_extension("git_worktree")
    end,
  },
}
