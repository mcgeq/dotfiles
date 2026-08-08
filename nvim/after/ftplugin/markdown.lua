vim.opt_local.wrap = true
vim.opt_local.linebreak = true
vim.opt_local.conceallevel = 2

local function continue_list()
  local cur = vim.api.nvim_win_get_cursor(0)
  local line = vim.api.nvim_get_current_line()

  local indent, num, delim, tail = line:match "^(%s*)(%d+)([.)])(.*)$"
  local bullet
  if not indent then indent, bullet, tail = line:match "^(%s*)([-*+])(.*)$" end
  if not indent or (tail ~= "" and not tail:match "^%s") then
    vim.api.nvim_feedkeys(vim.api.nvim_replace_termcodes("<CR>", true, false, true), "n", false)
    return
  end

  local content = tail:match "^%s*(.*)$"
  if tail:match "^%s" and content == "" then
    vim.api.nvim_buf_set_lines(0, cur[1] - 1, cur[1], false, { indent })
    vim.api.nvim_win_set_cursor(0, { cur[1], #indent })
    return
  end

  local prefix = indent .. (num and (tostring(tonumber(num) + 1) .. delim) or bullet) .. " "
  local after = line:sub(cur[2] + 1):gsub("^%s+", "")
  vim.api.nvim_buf_set_lines(0, cur[1] - 1, cur[1], false, { line, prefix .. after })
  vim.api.nvim_win_set_cursor(0, { cur[1] + 1, #prefix })
end

vim.keymap.set("i", "<CR>", continue_list, { buffer = 0 })
