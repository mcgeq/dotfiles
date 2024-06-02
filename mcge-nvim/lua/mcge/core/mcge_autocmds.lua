local cfg = require("mcge.core").config
local myAutoGroup = vim.api.nvim_create_augroup("myAutoGroup", {
  clear = true,
})

local autocmd = vim.api.nvim_create_autocmd

if cfg.enable_imselect then
  autocmd("InsertLeave", {
    group = myAutoGroup,
    callback = require("insis.utils.im-select").insertLeave,
  })

  autocmd("InsertEnter", {
    group = myAutoGroup,
    callback = require("insis.utils.im-select").insertEnter,
  })
end

-- auto insert mode when TermOpen
autocmd("TermOpen", {
  group = myAutoGroup,
  command = "startinsert",
})

-- format on save
autocmd("BufWritePre", {
  group = myAutoGroup,
  pattern = require("insis.env").getFormatOnSavePattern(),
  callback = function()
    vim.lsp.buf.format()
  end,
})

-- set *.mdx to filetype to markdown
autocmd({ "BufNewFile", "BufRead" }, {
  group = myAutoGroup,
  pattern = "*.mdx",
  command = "setfiletype markdown",
})

-- set wrap only in markdown
autocmd("FileType", {
  group = myAutoGroup,
  pattern = { "markdown" },
  callback = function()
    if cfg.markdown then
      vim.opt_local.wrap = cfg.markdown.wrap
      vim.wo.wrap = cfg.markdown.wrap
    end
  end,
})

-- highlight on yank
autocmd("TextYankPost", {
  callback = function()
    vim.highlight.on_yank()
  end,
  group = myAutoGroup,
  pattern = "*",
})

-- https://www.reddit.com/r/neovim/comments/zc720y/tip_to_manage_hlsearch/
vim.on_key(function(char)
  if vim.fn.mode() == "n" then
    vim.opt.hlsearch = vim.tbl_contains({ "<CR>", "n", "N", "*", "#", "?", "/" }, vim.fn.keytrans(char))
  end
end, vim.api.nvim_create_namespace("auto_hlsearch"))

-- do not continue comments when type o
autocmd("BufEnter", {
  group = myAutoGroup,
  pattern = "*",
  callback = function()
    vim.opt.formatoptions = vim.opt.formatoptions
      - "o" -- O and o, don't continue comments
      + "r" -- But do continue when pressing enter.
  end,
})

autocmd({ "FileType" }, {
  group = myAutoGroup,
  pattern = {
    "help",
    "man",
    "neotest-output",
  },
  callback = function()
    keymap({ "i", "n" }, { "q", "<esc>" }, "<esc>:close<CR>", { buffer = true })
  end,
})

-- save fold
autocmd("BufWinEnter", {
  group = myAutoGroup,
  pattern = "*",
  command = "silent! loadview",
})

autocmd("BufWrite", {
  group = myAutoGroup,
  pattern = "*",
  command = "mkview",
})
