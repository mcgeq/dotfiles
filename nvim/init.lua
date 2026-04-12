if vim.fn.has("nvim-0.12") == 0 then
  vim.api.nvim_echo({
    { "This configuration requires Neovim 0.12 or newer.", "ErrorMsg" },
  }, true, {})
  return
end

vim.g.mapleader = " "
vim.g.maplocalleader = ","

if vim.loader then vim.loader.enable() end

require("core")
require("pack").setup()
require("plugins").setup()
require("lsp").setup()
