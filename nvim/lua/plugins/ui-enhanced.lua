-- 现代化 UI 增强配置
-- 提供更美观的界面体验

---@type LazySpec
return {
  -- ===== 现代主题集合 =====
  -- 注意：只有当前使用的主题需要 lazy = false
  -- 其他主题设置为 lazy = true 以提升启动速度
  
  -- Catppuccin - 柔和优雅（备用主题，lazy 加载）
  {
    "catppuccin/nvim",
    name = "catppuccin",
    lazy = true, -- 优化：按需加载
    priority = 1000,
    opts = {
      flavour = "mocha", -- latte, frappe, macchiato, mocha
      transparent_background = false,
      show_end_of_buffer = false,
      term_colors = true,
      dim_inactive = {
        enabled = true,
        shade = "dark",
        percentage = 0.15,
      },
      integrations = {
        cmp = true,
        gitsigns = true,
        nvimtree = false,
        treesitter = true,
        mini = true,
        mason = true,
        noice = true,
        notify = true,
        which_key = true,
        telescope = false,
        flash = true,
        indent_blankline = {
          enabled = true,
          colored_indent_levels = true,
        },
      },
    },
  },

  -- Tokyo Night - 清爽现代（备用主题，lazy 加载）
  {
    "folke/tokyonight.nvim",
    lazy = true, -- 优化：按需加载
    priority = 1000,
    opts = {
      style = "night", -- storm, moon, night, day
      transparent = false,
      terminal_colors = true,
      styles = {
        comments = { italic = true },
        keywords = { italic = true },
        functions = {},
        variables = {},
        sidebars = "dark",
        floats = "dark",
      },
      sidebars = { "qf", "help", "terminal", "packer" },
      dim_inactive = true,
      lualine_bold = true,
      on_highlights = function(hl, c)
        hl.CursorLineNr = { fg = c.orange, bold = true }
      end,
    },
  },

  -- Kanagawa - 日式美学（备用主题，lazy 加载）
  {
    "rebelot/kanagawa.nvim",
    lazy = true, -- 优化：按需加载
    priority = 1000,
    opts = {
      compile = false,
      undercurl = true,
      commentStyle = { italic = true },
      functionStyle = {},
      keywordStyle = { italic = true },
      statementStyle = { bold = true },
      typeStyle = {},
      transparent = false,
      dimInactive = true,
      terminalColors = true,
      theme = "wave", -- wave, dragon, lotus
      background = {
        dark = "wave",
        light = "lotus",
      },
    },
  },

  -- ===== 通知系统 =====
  -- 注意：使用 Snacks Dashboard 作为启动画面（已在 snacks.nvim.lua 配置）
  {
    "rcarriga/nvim-notify",
    opts = {
      timeout = 3000,
      max_height = function()
        return math.floor(vim.o.lines * 0.75)
      end,
      max_width = function()
        return math.floor(vim.o.columns * 0.75)
      end,
      stages = "fade_in_slide_out",
      render = "compact",
      background_colour = "#000000",
      icons = {
        ERROR = "",
        WARN = "",
        INFO = "",
        DEBUG = "",
        TRACE = "✎",
      },
    },
  },

  -- ===== 平滑滚动 =====
  {
    "karb94/neoscroll.nvim",
    event = "VeryLazy",
    opts = {
      mappings = { "<C-u>", "<C-d>", "<C-b>", "<C-f>", "<C-y>", "<C-e>", "zt", "zz", "zb" },
      hide_cursor = true,
      stop_eof = true,
      respect_scrolloff = false,
      cursor_scrolls_alone = true,
      easing_function = "quadratic",
      pre_hook = nil,
      post_hook = nil,
    },
  },

  -- ===== 缩进线美化（增强版）=====
  {
    "lukas-reineke/indent-blankline.nvim",
    event = "BufReadPost",
    opts = {
      indent = {
        char = "│",
        tab_char = "│",
      },
      scope = {
        enabled = true,
        show_start = true,
        show_end = false,
        injected_languages = true,
        highlight = { "Function", "Label" },
        priority = 500,
      },
      exclude = {
        filetypes = {
          "help",
          "alpha",
          "dashboard",
          "neo-tree",
          "Trouble",
          "lazy",
          "mason",
          "notify",
          "toggleterm",
          "lazyterm",
        },
      },
    },
    main = "ibl",
  },

  -- ===== TODO 高亮和上下文显示 =====
  -- 注意：以下功能已由 AstroCommunity 提供
  --  - todo-comments.nvim (astrocommunity.editing-support.todo-comments-nvim)
  --  - nvim-treesitter-context (astrocommunity.editing-support.nvim-treesitter-context)
}
