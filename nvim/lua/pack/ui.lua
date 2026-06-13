local M = {
  lines = {},
  buf = nil,
  win = nil,
}

local function timestamp()
  return os.date("%H:%M:%S")
end

local function append(message, level)
  local line = string.format("[%s] %s", timestamp(), message)
  table.insert(M.lines, line)
  vim.schedule(function()
    vim.notify(message, level or vim.log.levels.INFO, { title = "vim.pack" })
    if M.buf and vim.api.nvim_buf_is_valid(M.buf) then
      vim.api.nvim_buf_set_lines(M.buf, -1, -1, false, { line })
      if M.win and vim.api.nvim_win_is_valid(M.win) then
        vim.api.nvim_win_set_cursor(M.win, { vim.api.nvim_buf_line_count(M.buf), 0 })
      end
    end
  end)
end

local function open_log_window()
  if M.buf and vim.api.nvim_buf_is_valid(M.buf) and M.win and vim.api.nvim_win_is_valid(M.win) then
    vim.api.nvim_set_current_win(M.win)
    return
  end

  M.buf = vim.api.nvim_create_buf(false, true)
  vim.bo[M.buf].bufhidden = "wipe"
  vim.bo[M.buf].buftype = "nofile"
  vim.bo[M.buf].filetype = "log"
  vim.api.nvim_buf_set_name(M.buf, "vim.pack://log")
  vim.api.nvim_buf_set_lines(M.buf, 0, -1, false, M.lines)

  local width = math.floor(vim.o.columns * 0.72)
  local height = math.max(12, math.floor(vim.o.lines * 0.45))
  local row = math.floor((vim.o.lines - height) / 2)
  local col = math.floor((vim.o.columns - width) / 2)

  M.win = vim.api.nvim_open_win(M.buf, true, {
    relative = "editor",
    border = "rounded",
    style = "minimal",
    title = " vim.pack log ",
    width = width,
    height = height,
    row = row,
    col = col,
  })

  vim.keymap.set("n", "q", function()
    if M.win and vim.api.nvim_win_is_valid(M.win) then vim.api.nvim_win_close(M.win, true) end
  end, { buffer = M.buf, silent = true, desc = "Close pack log" })
end

function M.setup()
  vim.api.nvim_create_autocmd("PackChangedPre", {
    callback = function(ev)
      local name = ev.data.spec.name
      local kind = ev.data.kind
      append(string.format("%s started: %s", kind, name))
    end,
  })

  vim.api.nvim_create_autocmd("PackChanged", {
    callback = function(ev)
      local name = ev.data.spec.name
      local kind = ev.data.kind
      append(string.format("%s finished: %s", kind, name))
    end,
  })

  vim.api.nvim_create_user_command("PackOpenLog", open_log_window, {
    desc = "Open vim.pack activity log",
  })
end

return M
