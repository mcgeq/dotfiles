local M = {}

local scratch = require "core.scratch"

function M.move_line_down()
  vim.cmd "move +1"
  vim.cmd "normal! =="
end

function M.move_line_up()
  vim.cmd "move -2"
  vim.cmd "normal! =="
end

function M.duplicate_line_below()
  local line = vim.api.nvim_get_current_line()
  local row = vim.api.nvim_win_get_cursor(0)[1]
  vim.api.nvim_buf_set_lines(0, row, row, false, { line })
  vim.api.nvim_win_set_cursor(0, { row + 1, 0 })
end

function M.open_scratch()
  scratch.open("scratch", nil, {
    filetype = "text",
    close_desc = "Close scratch buffer",
  })
end

return M
