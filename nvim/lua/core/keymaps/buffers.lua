local M = {}

local workspace = require "core.workspace"

local function command_exists(command) return vim.fn.exists(":" .. command) == 2 end

local function listed_buffers()
  local buffers = {}

  for _, buffer in ipairs(vim.fn.getbufinfo { buflisted = 1 }) do
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

function M.buffer_root(buffer) return workspace.project_root(buffer) or workspace.buffer_dir(buffer) end

local function delete_buffer(buffer, force) return pcall(vim.api.nvim_buf_delete, buffer, { force = force or false }) end

local function save_buffer(buffer)
  if vim.api.nvim_buf_get_name(buffer) == "" then return false end

  return pcall(vim.api.nvim_buf_call, buffer, function() vim.cmd.write() end)
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
    if notify_empty then vim.notify("No " .. action .. " to close.", vim.log.levels.INFO, { title = "Buffers" }) end
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
      local skip_delete = false

      if is_modified and strategy == "skip" then
        skipped = skipped + 1
        skip_delete = true
      elseif is_modified and strategy == "save" and not save_buffer(buffer) then
        table.insert(failed, buffer_label(buffer))
        skip_delete = true
      end

      if not skip_delete and not delete_buffer(buffer, strategy == "discard") then
        table.insert(failed, buffer_label(buffer))
      end
    end
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

function M.close_other()
  local current = vim.api.nvim_get_current_buf()
  local targets = {}

  for _, buffer in ipairs(ordered_buffers()) do
    if buffer ~= current then table.insert(targets, buffer) end
  end

  delete_buffers(targets, { action = "other buffers", notify_empty = true })
end

function M.close_left()
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

function M.close_right()
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

function M.notify_bufferline_feature(feature)
  vim.notify(feature .. " requires `bufferline.nvim`.", vim.log.levels.WARN, { title = "Buffers" })
end

function M.pick()
  if command_exists "BufferLinePick" then
    vim.cmd "BufferLinePick"
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
    format_item = function(item) return ("%d: %s"):format(item.index, item.label) end,
  }, function(choice)
    if choice and vim.api.nvim_buf_is_valid(choice.bufnr) then vim.api.nvim_set_current_buf(choice.bufnr) end
  end)
end

function M.close_current()
  delete_buffers({ vim.api.nvim_get_current_buf() }, {
    action = "the current buffer",
    allow_skip = false,
  })
end

function M.close_project()
  local current = vim.api.nvim_get_current_buf()
  local root = M.buffer_root(current)
  if not root then
    vim.notify("Current buffer has no project root yet.", vim.log.levels.WARN, { title = "Buffers" })
    return
  end

  local targets = {}
  for _, buffer in ipairs(ordered_buffers()) do
    if buffer ~= current and M.buffer_root(buffer) == root then table.insert(targets, buffer) end
  end

  delete_buffers(targets, {
    action = "other buffers in the current project",
    notify_empty = true,
  })
end

function M.goto_buffer(index)
  return function()
    if command_exists "BufferLineGoToBuffer" then
      vim.cmd(("BufferLineGoToBuffer %d"):format(index))
      return
    end

    local target = listed_buffers()[index]
    if target and vim.api.nvim_buf_is_valid(target) then vim.api.nvim_set_current_buf(target) end
  end
end

function M.alternate()
  local alternate = vim.fn.bufnr "#"
  if alternate < 1 or not vim.api.nvim_buf_is_valid(alternate) then
    vim.notify("No alternate buffer yet.", vim.log.levels.INFO, { title = "Buffers" })
    return
  end

  vim.cmd.buffer(alternate)
end

function M.bufferline_command(command, fallback)
  return function()
    if command_exists(command) then
      vim.cmd(command)
      return
    end

    fallback()
  end
end

return M
