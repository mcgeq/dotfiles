-- Markdown 增强配置
-- 提供完整的 Markdown 写作和预览体验

---@type LazySpec
return {
  -- ===== Markdown 预览 =====
  {
    "iamcco/markdown-preview.nvim",
    ft = "markdown",
    build = function()
      vim.fn["mkdp#util#install"]()
    end,
    keys = {
      { "<leader>mp", "<cmd>MarkdownPreviewToggle<cr>", desc = "Markdown Preview Toggle" },
      { "<leader>ms", "<cmd>MarkdownPreview<cr>", desc = "Markdown Preview Start" },
      { "<leader>mx", "<cmd>MarkdownPreviewStop<cr>", desc = "Markdown Preview Stop" },
    },
    config = function()
      vim.g.mkdp_auto_start = 0
      vim.g.mkdp_auto_close = 1
      vim.g.mkdp_refresh_slow = 0
      vim.g.mkdp_command_for_global = 0
      vim.g.mkdp_open_to_the_world = 0
      vim.g.mkdp_open_ip = ""
      vim.g.mkdp_browser = "" -- 使用系统默认浏览器
      vim.g.mkdp_echo_preview_url = 1
      vim.g.mkdp_browserfunc = ""
      
      -- 主题：github(亮), dark(暗)
      vim.g.mkdp_theme = "dark"
      
      -- 预览选项
      vim.g.mkdp_preview_options = {
        mkit = {},
        katex = {},
        uml = {},
        maid = {},
        disable_sync_scroll = 0,
        sync_scroll_type = "middle",
        hide_yaml_meta = 1,
        sequence_diagrams = {},
        flowchart_diagrams = {},
        content_editable = false,
        disable_filename = 0,
        toc = {},
      }
      
      -- 端口范围
      vim.g.mkdp_port = ""
      vim.g.mkdp_page_title = "「${name}」"
      vim.g.mkdp_filetypes = { "markdown" }
    end,
  },

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

  -- ===== Markdown 标题导航 =====
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
        fat_headlines = true,
        fat_headline_upper_string = "▃",
        fat_headline_lower_string = "🬂",
      },
    },
  },
}
