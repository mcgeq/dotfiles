-- Copy this file to `private.lua` (git-ignored) and fill in your own
-- machine-specific values. Keep secrets and local paths out of git here.
--
-- Merge order: defaults < lua/user/<section>.lua < this file.
-- So anything defined here overrides the corresponding `user.*` section on
-- this machine, and every `features` key below can be set to `false`.
return {
  -- ---------------------------------------------------------------------------
  -- Machine-level feature toggles. All keys default to enabled; set one to
  -- `false` to turn that feature off on this machine.
  -- ---------------------------------------------------------------------------
  features = {
    -- dashboard = false,      -- snacks dashboard 启动页
    -- bufferline = false,     -- bufferline 顶部标签栏
    -- noice = false,          -- noice 消息/命令行 UI
    -- nerd_font = false,      -- 使用 Nerd Font 图标
    -- debug = false,          -- DAP 调试 (codelldb / debugpy)
    -- database = false,       -- dadbod 数据库 UI
    -- go = true,              -- go.nvim 工具链（默认关闭，需要时打开）
    -- cpp = false,            -- clangd_extensions + cmake-tools
    -- frontend = false,       -- 前端 eslint / colorizer / autotag
    -- live_server = false,    -- live-server 终端
    -- web = false,            -- package-info + hurl
    -- spectre = false,        -- nvim-spectre 批量替换
    -- markdown_extras = false,-- markdown 表格/图片/TOC/render
    -- format_on_save = false, -- 保存时自动格式化
  },

  -- ---------------------------------------------------------------------------
  -- Optional overrides of the lua/user/* sections (same keys as the section).
  -- ---------------------------------------------------------------------------
  -- theme = {
  --   name = "kanagawa",
  --   overrides = { kanagawa = { theme = "dragon", transparent = true } },
  --   highlights = nil,
  -- },

  -- pack = {
  --   -- Disable base plugins you never use on this machine.
  --   disable = { "bufferline.nvim" },
  --   -- add = { { src = "https://github.com/...", name = "..." } },
  -- },

  -- session = {
  --   autoread = true, -- auto-restore the workspace session on empty startup
  --   autowrite = true,
  -- },

  -- lsp = {
  --   ensure_installed = { "clangd" }, -- extra Mason servers to pre-install
  --   auto_install = true,
  -- },

  -- lang = {
  --   -- frontend = { formatter = "conform" },
  --   -- mason = { "codelldb" },
  --   -- treesitter = { "html" },
  -- },

  -- ---------------------------------------------------------------------------
  -- Hugo blogging: enable the feature and point at your Hugo site root.
  -- Kept private so blog activation + directory paths never land in git.
  -- ---------------------------------------------------------------------------
  -- hugo = {
  --   enabled = true,
  --   blog_path = "D:\\path\\to\\your-hugo-blog",
  -- },
}
