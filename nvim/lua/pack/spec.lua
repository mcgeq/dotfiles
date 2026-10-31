local M = {}

function M.get_base_specs()
  return {
    { src = "https://github.com/nvim-tree/nvim-web-devicons", name = "nvim-web-devicons", start = true },
    { src = "https://github.com/catppuccin/nvim", name = "catppuccin", start = true },
    { src = "https://github.com/folke/which-key.nvim", name = "which-key.nvim", start = true },
    { src = "https://github.com/folke/tokyonight.nvim", name = "tokyonight.nvim", start = true },
    { src = "https://github.com/rebelot/kanagawa.nvim", name = "kanagawa.nvim", start = true },
    { src = "https://github.com/folke/snacks.nvim", name = "snacks.nvim", start = true },
    { src = "https://github.com/folke/flash.nvim", name = "flash.nvim", start = true },
    { src = "https://github.com/chrisgrieser/nvim-spider", name = "nvim-spider", start = true },
    { src = "https://github.com/echasnovski/mini.nvim", name = "mini.nvim", start = true },
    { src = "https://github.com/folke/noice.nvim", name = "noice.nvim", start = true, feature = "noice" },
    { src = "https://github.com/nvim-lualine/lualine.nvim", name = "lualine.nvim", start = true },
    {
      src = "https://github.com/akinsho/bufferline.nvim",
      name = "bufferline.nvim",
      start = true,
      feature = "bufferline",
    },
    { src = "https://github.com/lewis6991/gitsigns.nvim", name = "gitsigns.nvim", start = true },
    { src = "https://github.com/folke/trouble.nvim", name = "trouble.nvim", start = true },
    { src = "https://github.com/folke/todo-comments.nvim", name = "todo-comments.nvim", start = true },
    { src = "https://github.com/saghen/blink.lib", name = "blink.lib", start = true },
    { src = "https://github.com/saghen/blink.cmp", name = "blink.cmp", start = true },
    { src = "https://github.com/rafamadriz/friendly-snippets", name = "friendly-snippets", start = true },
    { src = "https://github.com/nvim-treesitter/nvim-treesitter", name = "nvim-treesitter", start = true },
    {
      src = "https://github.com/nvim-treesitter/nvim-treesitter-textobjects",
      name = "nvim-treesitter-textobjects",
      start = true,
    },
    {
      src = "https://github.com/nvim-treesitter/nvim-treesitter-context",
      name = "nvim-treesitter-context",
      start = true,
    },
    { src = "https://github.com/stevearc/conform.nvim", name = "conform.nvim", start = true },
    { src = "https://github.com/williamboman/mason.nvim", name = "mason.nvim", start = true },
    { src = "https://github.com/williamboman/mason-lspconfig.nvim", name = "mason-lspconfig.nvim", start = true },
    {
      src = "https://github.com/WhoIsSethDaniel/mason-tool-installer.nvim",
      name = "mason-tool-installer.nvim",
      start = true,
    },
    { src = "https://github.com/jay-babu/mason-nvim-dap.nvim", name = "mason-nvim-dap.nvim", feature = "debug" },
    { src = "https://github.com/neovim/nvim-lspconfig", name = "nvim-lspconfig", start = true },
    { src = "https://github.com/mfussenegger/nvim-dap", name = "nvim-dap", feature = "debug" },
    { src = "https://github.com/rcarriga/nvim-dap-ui", name = "nvim-dap-ui", feature = "debug" },
    { src = "https://github.com/theHamsta/nvim-dap-virtual-text", name = "nvim-dap-virtual-text", feature = "debug" },
    { src = "https://github.com/nvim-neotest/nvim-nio", name = "nvim-nio", feature = "debug" },
    { src = "https://github.com/b0o/SchemaStore.nvim", name = "SchemaStore.nvim", start = true },
    { src = "https://github.com/nvim-lua/plenary.nvim", name = "plenary.nvim", start = true },
    { src = "https://github.com/MunifTanjim/nui.nvim", name = "nui.nvim", start = true },
    { src = "https://github.com/dhruvasagar/vim-table-mode", name = "vim-table-mode", feature = "markdown_extras" },
    { src = "https://github.com/HakonHarnes/img-clip.nvim", name = "img-clip.nvim", feature = "markdown_extras" },
    { src = "https://github.com/mzlogin/vim-markdown-toc", name = "vim-markdown-toc", feature = "markdown_extras" },
    {
      src = "https://github.com/MeanderingProgrammer/render-markdown.nvim",
      name = "render-markdown.nvim",
      feature = "markdown_extras",
    },
    { src = "https://github.com/vuki656/package-info.nvim", name = "package-info.nvim", feature = "web" },
    { src = "https://github.com/nvim-pack/nvim-spectre", name = "nvim-spectre", feature = "spectre" },
    { src = "https://github.com/jellydn/hurl.nvim", name = "hurl.nvim", feature = "web" },
    { src = "https://github.com/ray-x/guihua.lua", name = "guihua.lua", feature = "go" },
    { src = "https://github.com/ray-x/go.nvim", name = "go.nvim", feature = "go" },
    { src = "https://github.com/tpope/vim-dadbod", name = "vim-dadbod", feature = "database" },
    {
      src = "https://github.com/kristijanhusak/vim-dadbod-completion",
      name = "vim-dadbod-completion",
      feature = "database",
    },
    { src = "https://github.com/kristijanhusak/vim-dadbod-ui", name = "vim-dadbod-ui", feature = "database" },
    { src = "https://github.com/NvChad/nvim-colorizer.lua", name = "nvim-colorizer.lua", feature = "frontend" },
    { src = "https://github.com/windwp/nvim-ts-autotag", name = "nvim-ts-autotag", feature = "frontend" },
  }
end

-- Keep lifecycle decisions beside inventory; native vim.pack only receives native fields.
function M.normalize(spec)
  if type(spec) == "string" then spec = { src = spec } end
  if type(spec) ~= "table" or not spec.src then return nil end
  spec = vim.deepcopy(spec)
  spec.name = spec.name or spec.src:gsub("/+$", ""):match("([^/]+)$"):gsub("%.git$", "")
  return spec
end

function M.extension_specs(extension)
  local specs = extension.spec or extension
  if not vim.islist(specs) then specs = { specs } end
  local entries = {}
  for _, spec in ipairs(specs) do
    spec = M.normalize(spec)
    if spec then
      spec.feature = spec.feature or extension.feature
      if spec.start == nil then spec.start = type(extension.setup) == "function" and extension.lazy ~= true end
      entries[#entries + 1] = spec
    end
  end
  return entries
end

function M.entries()
  local entries = M.get_base_specs()
  for _, extension in ipairs(require("core.util").load_table_modules "user.plugins") do
    vim.list_extend(entries, M.extension_specs(extension))
  end
  for _, spec in ipairs(require("user").pack.add) do
    if type(spec) == "string" then
      spec = { src = spec }
    else
      spec = vim.deepcopy(spec)
    end
    if spec.start == nil then spec.start = true end
    entries[#entries + 1] = spec
  end
  local normalized = {}
  for _, spec in ipairs(entries) do
    spec = M.normalize(spec)
    if spec then normalized[#normalized + 1] = spec end
  end
  return normalized
end

function M.enabled(name, entry)
  local user = require "user"
  if vim.list_contains(user.pack.disable, name) then return false end
  if not entry then
    for _, candidate in ipairs(M.entries()) do
      if candidate.name == name then
        entry = candidate
        break
      end
    end
  end
  return not entry or not entry.feature or user.private.features[entry.feature] ~= false
end

function M.native_spec(entry) return { src = entry.src, name = entry.name, version = entry.version, data = entry.data } end

function M.configurations()
  return {
    { module = "ui" },
    { module = "statusline", plugin = "lualine.nvim" },
    { module = "noice", plugin = "noice.nvim" },
    { module = "snacks", plugin = "snacks.nvim" },
    { module = "flash", plugin = "flash.nvim" },
    { module = "editing" },
    { module = "completion", plugin = "blink.cmp" },
    { module = "git", plugin = "gitsigns.nvim" },
    { module = "syntax" },
    { module = "format" },
    { module = "backend" },
    { module = "frontend", feature = "frontend" },
    { module = "search", plugin = "nvim-spectre" },
    { module = "web", feature = "web" },
    { module = "debug", feature = "debug" },
    { module = "workflow" },
    { module = "markdown" },
  }
end

return M
