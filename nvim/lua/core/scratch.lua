local M = {}

function M.create(name, opts)
  opts = opts or {}

  vim.cmd(opts.command or "botright new")

  local bufnr = vim.api.nvim_get_current_buf()
  vim.bo[bufnr].buftype = "nofile"
  vim.bo[bufnr].bufhidden = "wipe"
  vim.bo[bufnr].swapfile = false
  vim.bo[bufnr].buflisted = false
  vim.bo[bufnr].modifiable = opts.modifiable ~= false
  vim.bo[bufnr].filetype = opts.filetype or "text"
  vim.api.nvim_buf_set_name(bufnr, name)
  vim.keymap.set("n", "q", "<cmd>close<cr>", {
    buffer = bufnr,
    silent = true,
    desc = opts.close_desc or "Close window",
  })

  return bufnr
end

function M.set_lines(bufnr, lines)
  if not vim.api.nvim_buf_is_valid(bufnr) then return end

  vim.bo[bufnr].modifiable = true
  vim.api.nvim_buf_set_lines(bufnr, 0, -1, false, lines)
  vim.bo[bufnr].modifiable = false
end

function M.open(name, lines, opts)
  local bufnr = M.create(name, opts)

  if lines then
    M.set_lines(bufnr, lines)
    vim.api.nvim_win_set_cursor(0, { 1, 0 })
  end

  return bufnr
end

return M
