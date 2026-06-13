local M = {}
local workspace = require("core.workspace")
local scratch = require("core.scratch")

local window_zoom_state = {}

local function command_exists(command)
  return vim.fn.exists(":" .. command) == 2
end

local function listed_buffers()
  local buffers = {}

  for _, buffer in ipairs(vim.fn.getbufinfo({ buflisted = 1 })) do
    if vim.api.nvim_buf_is_valid(buffer.bufnr) and vim.bo[buffer.bufnr].buflisted then
      table.insert(buffers, buffer.bufnr)
    end
  end

  return buffers
end

local function ordered_buffers()
  local ok, bufferline = pcall(require, "bufferline")
  if ok and type(bufferline.get_elements) == "function" then
    local ok_elements, data = pcall(bufferline.get_elements)
    if ok_elements and type(data) == "table" and type(data.elements) == "table" then
      local buffers = {}
      for _, element in ipairs(data.elements) do
        local buffer = tonumber(element.id)
        if buffer and vim.api.nvim_buf_is_valid(buffer) and vim.bo[buffer].buflisted then
          table.insert(buffers, buffer)
        end
      end
      if #buffers > 0 then return buffers end
    end
  end

  return listed_buffers()
end

local function buffer_label(buffer)
  local name = vim.api.nvim_buf_get_name(buffer)
  if name == "" then return "[No Name]" end

  return vim.fn.fnamemodify(name, ":~:.")
end

local function buffer_root(buffer)
  return workspace.project_root(buffer) or workspace.buffer_dir(buffer)
end

local function delete_buffer(buffer, force)
  return pcall(vim.api.nvim_buf_delete, buffer, { force = force or false })
end

local function save_buffer(buffer)
  if vim.api.nvim_buf_get_name(buffer) == "" then return false end

  return pcall(vim.api.nvim_buf_call, buffer, function()
    vim.cmd.write()
  end)
end

local function confirm_modified_buffers(action, count, allow_skip)
  local prompt = ("%d modified buffer(s) detected while closing %s."):format(count, action)
  local buttons

  if allow_skip then
    buttons = "&Save and close\n&Discard changes\n&Skip modified\n&Cancel"
  else
    buttons = "&Save and close\n&Discard changes\n&Cancel"
  end

  local choice = vim.fn.confirm(prompt, buttons, 1, "Question")
  if allow_skip then
    if choice == 1 then return "save" end
    if choice == 2 then return "discard" end
    if choice == 3 then return "skip" end
    return "cancel"
  end

  if choice == 1 then return "save" end
  if choice == 2 then return "discard" end
  return "cancel"
end

local function delete_buffers(buffers, opts)
  opts = opts or {}
  local action = opts.action or "selected buffers"
  local allow_skip = opts.allow_skip ~= false
  local notify_empty = opts.notify_empty == true

  if #buffers == 0 then
    if notify_empty then
      vim.notify("No " .. action .. " to close.", vim.log.levels.INFO, { title = "Buffers" })
    end
    return
  end

  local modified = {}
  for _, buffer in ipairs(buffers) do
    if vim.api.nvim_buf_is_valid(buffer) and vim.bo[buffer].buflisted and vim.bo[buffer].modified then
      table.insert(modified, buffer)
    end
  end

  local strategy = "delete"
  if #modified > 0 then
    strategy = confirm_modified_buffers(action, #modified, allow_skip)
    if strategy == "cancel" then return end
  end

  local skipped = 0
  local failed = {}

  for _, buffer in ipairs(buffers) do
    if vim.api.nvim_buf_is_valid(buffer) and vim.bo[buffer].buflisted then
      local is_modified = vim.bo[buffer].modified
      if is_modified and strategy == "skip" then
        skipped = skipped + 1
      else
        if is_modified and strategy == "save" and not save_buffer(buffer) then
          table.insert(failed, buffer_label(buffer))
          goto continue
        end

        if not delete_buffer(buffer, strategy == "discard") then
          table.insert(failed, buffer_label(buffer))
        end
      end
    end

    ::continue::
  end

  if skipped > 0 then
    vim.notify(("Skipped %d modified buffer(s)."):format(skipped), vim.log.levels.INFO, { title = "Buffers" })
  end

  if #failed > 0 then
    vim.notify("Could not close: " .. table.concat(failed, ", "), vim.log.levels.WARN, { title = "Buffers" })
  end
end

local function buffer_index(buffers, target)
  for index, buffer in ipairs(buffers) do
    if buffer == target then return index end
  end

  return nil
end

local function close_other_buffers()
  local current = vim.api.nvim_get_current_buf()
  local targets = {}

  for _, buffer in ipairs(ordered_buffers()) do
    if buffer ~= current then table.insert(targets, buffer) end
  end

  delete_buffers(targets, { action = "other buffers", notify_empty = true })
end

local function close_left_buffers()
  local buffers = ordered_buffers()
  local current = vim.api.nvim_get_current_buf()
  local current_index = buffer_index(buffers, current)
  if not current_index or current_index <= 1 then return end

  local targets = {}
  for index = 1, current_index - 1 do
    table.insert(targets, buffers[index])
  end

  delete_buffers(targets, { action = "left buffers", notify_empty = true })
end

local function close_right_buffers()
  local buffers = ordered_buffers()
  local current = vim.api.nvim_get_current_buf()
  local current_index = buffer_index(buffers, current)
  if not current_index or current_index >= #buffers then return end

  local targets = {}
  for index = current_index + 1, #buffers do
    table.insert(targets, buffers[index])
  end

  delete_buffers(targets, { action = "right buffers", notify_empty = true })
end

local function notify_bufferline_feature(feature)
  vim.notify(feature .. " requires `bufferline.nvim`.", vim.log.levels.WARN, { title = "Buffers" })
end

local function pick_buffer()
  if command_exists("BufferLinePick") then
    vim.cmd("BufferLinePick")
    return
  end

  local ok, snacks = pcall(require, "snacks")
  if ok and snacks and snacks.picker and snacks.picker.buffers then
    snacks.picker.buffers()
    return
  end

  local items = {}
  for index, buffer in ipairs(ordered_buffers()) do
    table.insert(items, {
      index = index,
      bufnr = buffer,
      label = buffer_label(buffer),
    })
  end

  vim.ui.select(items, {
    prompt = "Buffers",
    format_item = function(item)
      return ("%d: %s"):format(item.index, item.label)
    end,
  }, function(choice)
    if choice and vim.api.nvim_buf_is_valid(choice.bufnr) then
      vim.api.nvim_set_current_buf(choice.bufnr)
    end
  end)
end

local function close_current_buffer()
  delete_buffers({ vim.api.nvim_get_current_buf() }, {
    action = "the current buffer",
    allow_skip = false,
  })
end

local function close_project_buffers()
  local current = vim.api.nvim_get_current_buf()
  local root = buffer_root(current)
  if not root then
    vim.notify("Current buffer has no project root yet.", vim.log.levels.WARN, { title = "Buffers" })
    return
  end

  local targets = {}
  for _, buffer in ipairs(ordered_buffers()) do
    if buffer ~= current and buffer_root(buffer) == root then
      table.insert(targets, buffer)
    end
  end

  delete_buffers(targets, {
    action = "other buffers in the current project",
    notify_empty = true,
  })
end

local function goto_buffer(index)
  return function()
    if command_exists("BufferLineGoToBuffer") then
      vim.cmd(("BufferLineGoToBuffer %d"):format(index))
      return
    end

    local target = listed_buffers()[index]
    if target and vim.api.nvim_buf_is_valid(target) then
      vim.api.nvim_set_current_buf(target)
    end
  end
end

local function current_tab_key()
  return tostring(vim.api.nvim_get_current_tabpage())
end

local function toggle_window_maximize()
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

  vim.cmd("wincmd |")
  vim.cmd("wincmd _")
end

local function close_other_windows()
  local ok = pcall(vim.cmd.only)
  if not ok then
    vim.notify("Could not close the other windows right now.", vim.log.levels.WARN, { title = "Windows" })
  end
end

local function move_window_to_new_tab()
  local ok = pcall(vim.cmd, "wincmd T")
  if not ok then
    vim.notify("Could not move the current window to a new tab.", vim.log.levels.WARN, { title = "Windows" })
  end
end

local function new_tab()
  vim.cmd.tabnew()
end

local function previous_tab()
  vim.cmd.tabprevious()
end

local function next_tab()
  vim.cmd.tabnext()
end

local function close_tab()
  local ok = pcall(vim.cmd.tabclose)
  if not ok then
    vim.notify("Could not close the current tab.", vim.log.levels.WARN, { title = "Tabs" })
  end
end

local function close_other_tabs()
  local ok = pcall(vim.cmd.tabonly)
  if not ok then
    vim.notify("Could not close the other tabs right now.", vim.log.levels.WARN, { title = "Tabs" })
  end
end

local function alternate_buffer()
  local alternate = vim.fn.bufnr("#")
  if alternate < 1 or not vim.api.nvim_buf_is_valid(alternate) then
    vim.notify("No alternate buffer yet.", vim.log.levels.INFO, { title = "Buffers" })
    return
  end

  vim.cmd.buffer(alternate)
end

local function quickfix_prev()
  local ok = pcall(vim.cmd.cprevious)
  if not ok then
    vim.notify("No previous quickfix item.", vim.log.levels.INFO, { title = "Quickfix" })
  end
end

local function quickfix_next()
  local ok = pcall(vim.cmd.cnext)
  if not ok then
    vim.notify("No next quickfix item.", vim.log.levels.INFO, { title = "Quickfix" })
  end
end

local function loclist_prev()
  local ok = pcall(vim.cmd.lprevious)
  if not ok then
    vim.notify("No previous location list item.", vim.log.levels.INFO, { title = "Location List" })
  end
end

local function loclist_next()
  local ok = pcall(vim.cmd.lnext)
  if not ok then
    vim.notify("No next location list item.", vim.log.levels.INFO, { title = "Location List" })
  end
end

local function clear_quickfix()
  vim.fn.setqflist({})
  pcall(vim.cmd.cclose)
  vim.notify("Quickfix list cleared.", vim.log.levels.INFO, { title = "Quickfix" })
end

local function clear_loclist()
  vim.fn.setloclist(0, {})
  pcall(vim.cmd.lclose)
  vim.notify("Location list cleared.", vim.log.levels.INFO, { title = "Location List" })
end

local function move_line_down()
  vim.cmd("move +1")
  vim.cmd("normal! ==")
end

local function move_line_up()
  vim.cmd("move -2")
  vim.cmd("normal! ==")
end

local function duplicate_line_below()
  local line = vim.api.nvim_get_current_line()
  local row = vim.api.nvim_win_get_cursor(0)[1]
  vim.api.nvim_buf_set_lines(0, row, row, false, { line })
  vim.api.nvim_win_set_cursor(0, { row + 1, 0 })
end

local function open_scratch_buffer()
  scratch.open("scratch", nil, {
    filetype = "text",
    close_desc = "Close scratch buffer",
  })
end

local function bufferline_command(command, fallback)
  return function()
    if command_exists(command) then
      vim.cmd(command)
      return
    end

    fallback()
  end
end

---@param mode string|string[]
---@param lhs string
---@param rhs string|function
---@param desc string
---@param opts table|nil
function M.map(mode, lhs, rhs, desc, opts)
  opts = opts or {}
  opts.desc = desc
  opts.silent = opts.silent ~= false
  vim.keymap.set(mode, lhs, rhs, opts)
end

function M.setup()
  local map = M.map
  local sessions = require("core.sessions")

  map({ "n", "i", "v" }, "<C-s>", function()
    vim.cmd.write()
  end, "Save file")

  map("n", "<leader>qq", "<cmd>qa<cr>", "Quit all")
  map("n", "<leader>qw", "<cmd>wq<cr>", "Save and quit")
  map("n", "<leader>bb", pick_buffer, "Pick buffer")
  map("n", "<leader>bd", close_current_buffer, "Delete buffer")
  map("n", "<leader>bo", close_other_buffers, "Delete other buffers")
  map("n", "<leader>bP", close_project_buffers, "Delete project buffers")
  map("n", "<leader><tab>", alternate_buffer, "Alternate buffer")
  map("n", "<leader>bh", close_left_buffers, "Delete left buffers")
  map("n", "<leader>bl", close_right_buffers, "Delete right buffers")
  map("n", "<leader>bn", "<cmd>bnext<cr>", "Next buffer")
  map("n", "<leader>bp", "<cmd>bprevious<cr>", "Previous buffer")
  map("n", "<leader>bt", bufferline_command("BufferLineTogglePin", function()
    notify_bufferline_feature("Buffer pinning")
  end), "Toggle buffer pin")
  map("n", "<leader>bH", bufferline_command("BufferLineMovePrev", function()
    notify_bufferline_feature("Buffer reordering")
  end), "Move buffer left")
  map("n", "<leader>bL", bufferline_command("BufferLineMoveNext", function()
    notify_bufferline_feature("Buffer reordering")
  end), "Move buffer right")

  for index = 1, 9 do
    map("n", ("<leader>b%d"):format(index), goto_buffer(index), ("Go to buffer %d"):format(index))
  end

  map("n", "<C-h>", "<C-w>h", "Focus left window")
  map("n", "<C-j>", "<C-w>j", "Focus lower window")
  map("n", "<C-k>", "<C-w>k", "Focus upper window")
  map("n", "<C-l>", "<C-w>l", "Focus right window")

  map("n", "<leader>wm", toggle_window_maximize, "Maximize window")
  map("n", "<leader>wo", close_other_windows, "Close other windows")
  map("n", "<leader>wv", "<C-w>v", "Split vertically")
  map("n", "<leader>ws", "<C-w>s", "Split horizontally")
  map("n", "<leader>wq", "<C-w>q", "Close window")
  map("n", "<leader>wT", move_window_to_new_tab, "Move window to new tab")
  map("n", "<leader>w=", "<C-w>=", "Balance windows")
  map("n", "<leader>tn", new_tab, "New tab")
  map("n", "<leader>th", previous_tab, "Previous tab")
  map("n", "<leader>tl", next_tab, "Next tab")
  map("n", "<leader>tq", close_tab, "Close tab")
  map("n", "<leader>to", close_other_tabs, "Close other tabs")
  map("n", "[q", quickfix_prev, "Previous quickfix item")
  map("n", "]q", quickfix_next, "Next quickfix item")
  map("n", "[l", loclist_prev, "Previous location list item")
  map("n", "]l", loclist_next, "Next location list item")
  map("n", "<leader>xc", clear_quickfix, "Clear quickfix list")
  map("n", "<leader>xC", clear_loclist, "Clear location list")
  map("n", "<leader>od", duplicate_line_below, "Duplicate line below")
  map("n", "<leader>os", open_scratch_buffer, "Open scratch buffer")
  map("n", "<leader>nn", "<cmd>NvimStatus<cr>", "Nvim status")
  map("n", "<leader>nc", "<cmd>NvimConfig<cr>", "Open nvim config")
  map("n", "<leader>nC", "<cmd>NvimUserConfig<cr>", "Open user config")
  map("n", "<leader>nh", "<cmd>Health<cr>", "Run health check")
  map("n", "<leader>ni", "<cmd>LspInfo<cr>", "LSP info")
  map("n", "<leader>nl", "<cmd>LspLog<cr>", "LSP log")
  map("n", "<leader>nL", "<cmd>LspLogRaw<cr>", "Raw LSP log")
  map("n", "<leader>nr", "<cmd>RunnerKeys<cr>", "Runner key summary")
  map("n", "<leader>nm", "<cmd>Mason<cr>", "Open Mason")
  map("n", "<leader>nR", "<cmd>LspRestart<cr>", "Restart buffer LSP")
  map("n", "<leader>nM", "<cmd>messages<cr>", "Show messages")
  map("n", "<leader>mi", sessions.show_current, "Session info")
  map("n", "<leader>ms", sessions.save_current, "Save workspace session")
  map("n", "<leader>ml", sessions.load_current, "Load workspace session")
  map("n", "<leader>mS", function() sessions.select("read") end, "Select workspace session")
  map("n", "<leader>md", sessions.delete_current, "Delete workspace session")
  map("n", "<leader>mD", function() sessions.select("delete") end, "Select session to delete")
  map("n", "<leader>mr", sessions.restart, "Restart with workspace session")
  map("n", "<leader>Pi", workspace.show_info, "Project info")
  map("n", "<leader>Pr", workspace.cd_to_current_root, "Set cwd to current project root")
  map("n", "<leader>Pp", workspace.select, "Select project")
  map("n", "<leader>PS", function() workspace.select({ load_session = true }) end, "Select project + session")
  map("n", "<leader>uf", "<cmd>FrontendModeCycle<cr>", "Cycle frontend mode")
  map("n", "<leader>uF", "<cmd>FrontendMode<cr>", "Show frontend mode")
  map("n", "<leader>ut", "<cmd>ThemeCycle<cr>", "Cycle theme")
  map("n", "<leader>uT", "<cmd>Theme<cr>", "Show theme")

  map("n", "<Esc>", "<cmd>nohlsearch<cr>", "Clear search highlight")
  map("t", "<Esc><Esc>", "<C-\\><C-n>", "Exit terminal mode")
  map("n", "<A-j>", move_line_down, "Move line down")
  map("n", "<A-k>", move_line_up, "Move line up")
  map("i", "<A-j>", "<Esc><cmd>move +1<cr>==gi", "Move line down")
  map("i", "<A-k>", "<Esc><cmd>move -2<cr>==gi", "Move line up")
  map("v", "<A-j>", ":move '>+1<cr>gv=gv", "Move selection down")
  map("v", "<A-k>", ":move '<-2<cr>gv=gv", "Move selection up")
  map("v", "<leader>od", "y`>p`[v`]", "Duplicate selection below", { silent = true })
  map("v", "<", "<gv", "Indent left")
  map("v", ">", ">gv", "Indent right")

  local user_keymaps = require("core.util").require_if_exists("user.keymaps")
  if type(user_keymaps) == "function" then user_keymaps(M) end
end

return M
