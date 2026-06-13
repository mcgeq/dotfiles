local M = {}
local last_terminal = nil
local workspace = require("core.workspace")

local util = require("core.util")
local is_windows = util.is_windows

local function current_cwd()
  return vim.uv.cwd() or vim.fn.getcwd()
end

local function normalize_dir(path)
  return workspace.normalize(path) or workspace.normalize(current_cwd())
end

local function dir_label(path)
  return workspace.dir_label(path)
end

local function default_shell()
  if is_windows() then
    if vim.fn.executable("pwsh") == 1 then return "pwsh" end
    if vim.fn.executable("powershell") == 1 then return "powershell" end
    return "cmd.exe"
  end

  if vim.env.SHELL and vim.env.SHELL ~= "" then return vim.env.SHELL end
  return vim.o.shell ~= "" and vim.o.shell or "sh"
end

local function resolve_terminal_command(cmd)
  if cmd == nil then return default_shell() end
  if type(cmd) == "string" and cmd == "" then return default_shell() end
  if type(cmd) == "table" and vim.tbl_isempty(cmd) then return default_shell() end
  return cmd
end

local function remember_terminal(cmd, opts)
  last_terminal = {
    cmd = vim.deepcopy(cmd),
    opts = vim.deepcopy(opts or {}),
  }
end

local function terminal_title(prefix, cwd)
  return (" %s: %s "):format(prefix, dir_label(cwd))
end

function M.cwd()
  return workspace.cwd()
end

function M.buffer_dir(bufnr)
  return workspace.buffer_dir(bufnr)
end

function M.project_root(bufnr)
  return workspace.project_root(bufnr)
end

---@param cmd string|string[]
---@param opts table|nil
---@return integer, integer
function M.open_float(cmd, opts)
  opts = opts or {}
  local resolved_cmd = resolve_terminal_command(cmd)
  local resolved_cwd = normalize_dir(opts.cwd)
  local buf = vim.api.nvim_create_buf(false, true)
  local width = math.floor(vim.o.columns * (opts.width_ratio or 0.9))
  local height = math.floor(vim.o.lines * (opts.height_ratio or 0.85))
  local row = math.floor((vim.o.lines - height) / 2)
  local col = math.floor((vim.o.columns - width) / 2)

  remember_terminal(resolved_cmd, vim.tbl_extend("force", {}, opts, { cwd = resolved_cwd }))

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

  vim.bo[buf].buflisted = false
  vim.bo[buf].bufhidden = "wipe"

  vim.fn.termopen(resolved_cmd, {
    cwd = resolved_cwd,
    env = opts.env,
  })
  if opts.enter_insert ~= false then vim.cmd.startinsert() end

  vim.keymap.set("n", "q", function()
    if vim.api.nvim_win_is_valid(win) then vim.api.nvim_win_close(win, true) end
  end, { buffer = buf, silent = true, desc = "Close terminal" })

  return buf, win
end

function M.open_cwd(cmd, opts)
  opts = vim.tbl_extend("force", {
    cwd = M.cwd(),
    title = terminal_title("shell", M.cwd()),
  }, opts or {})
  return M.open_float(cmd, opts)
end

function M.open_buffer_dir(cmd, opts)
  opts = opts or {}
  local bufnr = opts.bufnr or vim.api.nvim_get_current_buf()
  local dir = M.buffer_dir(bufnr)

  if not dir then
    dir = M.cwd()
    vim.notify("Current buffer has no file path yet; opening a shell in the current working directory.", vim.log.levels.INFO, {
      title = "Terminal",
    })
  end

  opts = vim.tbl_extend("force", {
    cwd = dir,
    title = terminal_title("buffer", dir),
  }, opts)
  opts.bufnr = nil
  return M.open_float(cmd, opts)
end

function M.open_project(cmd, opts)
  opts = opts or {}
  local bufnr = opts.bufnr or vim.api.nvim_get_current_buf()
  local root = M.project_root(bufnr)

  if not root then
    root = M.buffer_dir(bufnr) or M.cwd()
    vim.notify("No project root detected for the current buffer; opening a shell in the nearest available directory.", vim.log.levels.INFO, {
      title = "Terminal",
    })
  end

  opts = vim.tbl_extend("force", {
    cwd = root,
    title = terminal_title("project", root),
  }, opts)
  opts.bufnr = nil
  return M.open_float(cmd, opts)
end

function M.open_last()
  if not last_terminal then
    vim.notify("No floating terminal command has been opened yet.", vim.log.levels.INFO, { title = "Terminal" })
    return
  end

  return M.open_float(vim.deepcopy(last_terminal.cmd), vim.deepcopy(last_terminal.opts))
end

return M
