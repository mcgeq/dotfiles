local M = {}

local window_zoom_state = {}

local function current_tab_key() return tostring(vim.api.nvim_get_current_tabpage()) end

function M.toggle_maximize()
  local wins = vim.api.nvim_tabpage_list_wins(0)
  if #wins <= 1 then return end

  local current = vim.api.nvim_get_current_win()
  local tab_key = current_tab_key()
  local state = window_zoom_state[tab_key]

  if state and state.restore then
    pcall(vim.cmd, state.restore)
    window_zoom_state[tab_key] = nil
    if state.win == current then return end
  end

  window_zoom_state[tab_key] = {
    win = current,
    restore = vim.fn.winrestcmd(),
  }

  vim.cmd "wincmd |"
  vim.cmd "wincmd _"
end

function M.close_other_windows()
  local ok = pcall(vim.cmd.only)
  if not ok then
    vim.notify("Could not close the other windows right now.", vim.log.levels.WARN, { title = "Windows" })
  end
end

function M.move_window_to_new_tab()
  local ok = pcall(vim.cmd, "wincmd T")
  if not ok then
    vim.notify("Could not move the current window to a new tab.", vim.log.levels.WARN, { title = "Windows" })
  end
end

function M.new_tab() vim.cmd.tabnew() end

function M.previous_tab() vim.cmd.tabprevious() end

function M.next_tab() vim.cmd.tabnext() end

function M.close_tab()
  local ok = pcall(vim.cmd.tabclose)
  if not ok then vim.notify("Could not close the current tab.", vim.log.levels.WARN, { title = "Tabs" }) end
end

function M.close_other_tabs()
  local ok = pcall(vim.cmd.tabonly)
  if not ok then vim.notify("Could not close the other tabs right now.", vim.log.levels.WARN, { title = "Tabs" }) end
end

return M
