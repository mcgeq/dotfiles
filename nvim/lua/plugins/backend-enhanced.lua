-- 后端开发增强配置
-- 提供更好的后端开发体验
-- 支持：Rust, Go, Python

-- 注意：C/C++, Zig 支持已由 AstroNvim pack 提供
--  - astrocommunity.pack.cpp (包含 clangd LSP)
--  - astrocommunity.pack.zig (包含 zls LSP + zig.vim)

---@type LazySpec
return {
  -- ===== Rust 工具增强 =====
  {
    "Saecki/crates.nvim",
    event = { "BufRead Cargo.toml" },
    dependencies = { "nvim-lua/plenary.nvim" },
    opts = {
      popup = {
        autofocus = true,
        border = "rounded",
        show_version_date = true,
      },
      null_ls = {
        enabled = true,
        name = "crates.nvim",
      },
      lsp = {
        enabled = true,
        actions = true,
        completion = true,
        hover = true,
      },
    },
    keys = {
      { "<leader>cu", function() require("crates").update_crate() end, desc = "Update crate" },
      { "<leader>cU", function() require("crates").upgrade_crate() end, desc = "Upgrade crate" },
      { "<leader>ca", function() require("crates").update_all_crates() end, desc = "Update all crates" },
      { "<leader>cA", function() require("crates").upgrade_all_crates() end, desc = "Upgrade all crates" },
      { "<leader>cH", function() require("crates").open_homepage() end, desc = "Open crate homepage" },
      { "<leader>cD", function() require("crates").open_documentation() end, desc = "Open crate documentation" },
    },
  },

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
  {
    "linux-cultist/venv-selector.nvim",
    ft = "python",
    branch = "regexp",
    dependencies = {
      "neovim/nvim-lspconfig",
      "nvim-telescope/telescope.nvim",
      "mfussenegger/nvim-dap-python",
    },
    opts = {
      auto_refresh = true,
      search_venv_managers = true,
      search_workspace = true,
      search = true,
      dap_enabled = true,
      name = {
        "venv",
        ".venv",
        "env",
        ".env",
      },
    },
    keys = {
      { "<leader>vs", "<cmd>VenvSelect<cr>", desc = "Select Python venv" },
      { "<leader>vc", "<cmd>VenvSelectCached<cr>", desc = "Select cached venv" },
    },
  },

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
