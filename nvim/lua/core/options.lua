local opt = vim.opt

opt.number = true
opt.relativenumber = true
opt.signcolumn = "yes"
opt.wrap = false
opt.linebreak = true
opt.cursorline = true
opt.termguicolors = true
opt.laststatus = 3
opt.showmode = false
opt.splitright = true
opt.splitbelow = true
opt.scrolloff = 6
opt.sidescrolloff = 8
opt.ignorecase = true
opt.smartcase = true
opt.smartindent = true
opt.expandtab = true
opt.shiftwidth = 2
opt.tabstop = 2
opt.softtabstop = 2
opt.clipboard = "unnamedplus"
opt.updatetime = 200
opt.timeoutlen = 300
opt.completeopt = { "menu", "menuone", "noselect", "popup" }
opt.winborder = "rounded"
opt.confirm = true
opt.undofile = true
opt.swapfile = false
opt.backup = false
opt.mouse = "a"
opt.foldlevel = 99
opt.foldlevelstart = 99
opt.foldenable = true
opt.fillchars = {
  eob = " ",
  fold = " ",
  foldopen = "v",
  foldclose = ">",
  foldsep = " ",
}

vim.g.have_nerd_font = true

local user_options = require("core.util").require_if_exists("user.options")
if type(user_options) == "function" then user_options() end
