local augroup = vim.api.nvim_create_augroup
local autocmd = vim.api.nvim_create_autocmd

autocmd("TextYankPost", {
  group = augroup("core_yank_highlight", { clear = true }),
  callback = function() vim.highlight.on_yank() end,
})

autocmd("BufReadPost", {
  group = augroup("core_restore_cursor", { clear = true }),
  callback = function(args)
    local mark = vim.api.nvim_buf_get_mark(args.buf, '"')
    local line_count = vim.api.nvim_buf_line_count(args.buf)
    if mark[1] > 1 and mark[1] <= line_count then
      pcall(vim.api.nvim_win_set_cursor, 0, mark)
    end
  end,
})

autocmd("VimResized", {
  group = augroup("core_resize_splits", { clear = true }),
  command = "tabdo wincmd =",
})

autocmd("FileType", {
  group = augroup("core_close_with_q", { clear = true }),
  pattern = { "help", "man", "qf", "checkhealth", "lspinfo", "notify" },
  callback = function(event)
    vim.bo[event.buf].buflisted = false
    vim.keymap.set("n", "q", "<cmd>close<cr>", { buffer = event.buf, silent = true, desc = "Close window" })
  end,
})

local user_autocmds = require("core.util").require_if_exists("user.autocmds")
if type(user_autocmds) == "function" then user_autocmds() end
