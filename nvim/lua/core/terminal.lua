local M = {}

---@param cmd string|string[]
---@param opts table|nil
---@return integer, integer
function M.open_float(cmd, opts)
  opts = opts or {}
  local buf = vim.api.nvim_create_buf(false, true)
  local width = math.floor(vim.o.columns * (opts.width_ratio or 0.9))
  local height = math.floor(vim.o.lines * (opts.height_ratio or 0.85))
  local row = math.floor((vim.o.lines - height) / 2)
  local col = math.floor((vim.o.columns - width) / 2)

  local win = vim.api.nvim_open_win(buf, true, {
    relative = "editor",
    border = opts.border or "rounded",
    style = "minimal",
    title = opts.title or " terminal ",
    width = width,
    height = height,
    row = row,
    col = col,
  })

  vim.fn.termopen(cmd, {
    cwd = opts.cwd or vim.uv.cwd(),
  })
  vim.cmd.startinsert()

  vim.keymap.set("n", "q", function()
    if vim.api.nvim_win_is_valid(win) then vim.api.nvim_win_close(win, true) end
  end, { buffer = buf, silent = true, desc = "Close terminal" })

  return buf, win
end

return M
