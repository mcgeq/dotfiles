local M = {}

local DEFAULTS = {
  table = {
    format_on_insert_leave = true,
    format_on_pipe = true,
    notify_on_manual_format_failure = true,
  },
}

local function config()
  local user_lang = require("core.util").require_if_exists("user.lang") or {}
  return vim.tbl_deep_extend("force", {}, DEFAULTS, user_lang.markdown or {})
end

local function trim(text)
  return (text:gsub("^%s+", ""):gsub("%s+$", ""))
end

local function is_table_line(line)
  return line ~= nil and line:match("^%s*|.*|%s*$") ~= nil
end

local function split_cells(line)
  local cells = {}
  local body = line:gsub("^%s*|", ""):gsub("|%s*$", "")

  for cell in (body .. "|"):gmatch("(.-)|") do
    table.insert(cells, trim(cell))
  end

  return cells
end

local function is_separator_cell(cell)
  return cell:match("^:?-+:?$") ~= nil
end

local function format_separator(cell, width)
  local left = cell:sub(1, 1) == ":"
  local right = cell:sub(-1) == ":"
  local fill = string.rep("-", math.max(width, 1))
  return (left and ":" or "-") .. fill .. (right and ":" or "-")
end

local function find_table_bounds(bufnr, row)
  local last = vim.api.nvim_buf_line_count(bufnr)
  local first = row
  local final = row

  while first > 1 and is_table_line(vim.api.nvim_buf_get_lines(bufnr, first - 2, first - 1, false)[1]) do
    first = first - 1
  end

  while final < last and is_table_line(vim.api.nvim_buf_get_lines(bufnr, final, final + 1, false)[1]) do
    final = final + 1
  end

  return first, final
end

local function read_table(bufnr, first, final)
  local lines = vim.api.nvim_buf_get_lines(bufnr, first - 1, final, false)
  if #lines < 2 then return end

  local rows = {}
  local width = 0

  for _, line in ipairs(lines) do
    local cells = split_cells(line)
    width = math.max(width, #cells)
    table.insert(rows, cells)
  end

  for _, cells in ipairs(rows) do
    while #cells < width do
      table.insert(cells, "")
    end
  end

  local separator = rows[2]
  for _, cell in ipairs(separator) do
    if not is_separator_cell(cell) then return end
  end

  return rows, width
end

local function build_lines(rows, column_count)
  local widths = {}
  for index = 1, column_count do
    widths[index] = 0
  end

  for row_index, cells in ipairs(rows) do
    if row_index ~= 2 then
      for column = 1, column_count do
        widths[column] = math.max(widths[column], vim.fn.strdisplaywidth(cells[column] or ""))
      end
    end
  end

  local lines = {}

  for row_index, cells in ipairs(rows) do
    local parts = { "|" }
    for column = 1, column_count do
      local cell = cells[column] or ""
      if row_index == 2 then
        table.insert(parts, format_separator(cell, widths[column]))
      else
        local padding = widths[column] - vim.fn.strdisplaywidth(cell)
        table.insert(parts, " " .. cell .. string.rep(" ", padding) .. " ")
      end
      table.insert(parts, "|")
    end
    table.insert(lines, table.concat(parts))
  end

  return lines
end

function M.format_table(bufnr)
  bufnr = bufnr or 0
  bufnr = bufnr == 0 and vim.api.nvim_get_current_buf() or bufnr
  if vim.bo[bufnr].filetype ~= "markdown" then return false end

  local row = vim.api.nvim_win_get_cursor(0)[1]
  local line = vim.api.nvim_buf_get_lines(bufnr, row - 1, row, false)[1]
  if not is_table_line(line) then return false end

  local first, final = find_table_bounds(bufnr, row)
  local rows, column_count = read_table(bufnr, first, final)
  if not rows then return false end

  vim.api.nvim_buf_set_lines(bufnr, first - 1, final, false, build_lines(rows, column_count))
  return true
end

local function format_after_pipe(bufnr)
  local line = vim.api.nvim_get_current_line()
  local cursor = vim.api.nvim_win_get_cursor(0)
  local column = cursor[2]
  if column == 0 or line:sub(column, column) ~= "|" then return end
  if not M.format_table(bufnr) then return end
  vim.api.nvim_win_set_cursor(0, { cursor[1], #vim.api.nvim_get_current_line() })
end

function M.setup_buffer(bufnr)
  if vim.b[bufnr].markdown_table_setup then return end
  vim.b[bufnr].markdown_table_setup = true

  local cfg = config().table
  local group = vim.api.nvim_create_augroup("lang_markdown_table_" .. bufnr, { clear = true })

  if cfg.format_on_insert_leave then
    vim.api.nvim_create_autocmd("InsertLeave", {
      group = group,
      buffer = bufnr,
      callback = function()
        pcall(M.format_table, bufnr)
      end,
    })
  end

  if cfg.format_on_pipe then
    vim.keymap.set("i", "|", function()
      vim.schedule(function()
        if vim.api.nvim_buf_is_valid(bufnr) and vim.api.nvim_get_current_buf() == bufnr then
          pcall(format_after_pipe, bufnr)
        end
      end)
      return "|"
    end, {
      buffer = bufnr,
      desc = "Markdown table pipe",
      expr = true,
      silent = true,
    })
  end
end

function M.setup()
  vim.api.nvim_create_autocmd("FileType", {
    group = vim.api.nvim_create_augroup("lang_markdown_setup", { clear = true }),
    pattern = "markdown",
    callback = function(event)
      M.setup_buffer(event.buf)
    end,
  })
end

function M.manual_format()
  if M.format_table(0) then return end
  if config().table.notify_on_manual_format_failure then
    vim.notify("Cursor is not inside a valid markdown table.", vim.log.levels.WARN, { title = "Markdown" })
  end
end

return M
