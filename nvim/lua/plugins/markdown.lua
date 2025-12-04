-- Markdown 增强配置
-- 提供完整的 Markdown 写作和预览体验

-- 注意：Markdown 预览已由 AstroCommunity 提供
--  - astrocommunity.markdown-and-latex.markdown-preview-nvim
-- 如需自定义配置，可以在 polish.lua 中覆盖 vim.g.mkdp_* 选项

---@type LazySpec
return {

  -- ===== Markdown 表格助手 =====
  {
    "dhruvasagar/vim-table-mode",
    ft = "markdown",
    keys = {
      { "<leader>mt", "<cmd>TableModeToggle<cr>", desc = "Toggle Table Mode" },
    },
    config = function()
      vim.g.table_mode_corner = "|"
      vim.g.table_mode_corner_corner = "|"
      vim.g.table_mode_header_fillchar = "-"
    end,
  },

  -- ===== Markdown 图片粘贴 =====
  {
    "HakonHarnes/img-clip.nvim",
    ft = "markdown",
    keys = {
      { "<leader>mi", "<cmd>PasteImage<cr>", desc = "Paste Image" },
    },
    opts = {
      default = {
        dir_path = "assets/images", -- 图片保存目录
        file_name = "%Y-%m-%d-%H-%M-%S", -- 文件名格式
        use_absolute_path = false,
        relative_to_current_file = true,
      },
    },
  },

  -- ===== Markdown 目录生成 =====
  {
    "mzlogin/vim-markdown-toc",
    ft = "markdown",
    cmd = { "GenTocGFM", "GenTocGitLab", "GenTocMarked" },
    keys = {
      { "<leader>mT", "<cmd>GenTocGFM<cr>", desc = "Generate TOC (GitHub)" },
    },
  },

  -- ===== Markdown 标题美化 =====
  {
    "lukas-reineke/headlines.nvim",
    ft = "markdown",
    dependencies = "nvim-treesitter/nvim-treesitter",
    opts = {
      markdown = {
        headline_highlights = {
          "Headline1",
          "Headline2",
          "Headline3",
          "Headline4",
          "Headline5",
          "Headline6",
        },
        fat_headlines = false, -- 禁用上下装饰线
        fat_headline_upper_string = "",
        fat_headline_lower_string = "",
      },
    },
    config = function(_, opts)
      require("headlines").setup(opts)
      
      -- 自定义标题背景颜色（类似官方示例）
      vim.cmd([[
        highlight Headline1 guibg=#1e2718
        highlight Headline2 guibg=#21262d
        highlight Headline3 guibg=#1a1e30
        highlight Headline4 guibg=#1e2030
        highlight Headline5 guibg=#1d1f2b
        highlight Headline6 guibg=#1c1c1c
      ]])
    end,
  },
}
