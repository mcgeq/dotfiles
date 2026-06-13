local M = {}
local workspace = require("core.workspace")
local SESSION_ROOT_PREFIX = '" workspace_root: '
local session_file_path

local function user_session_config()
  local util = require("core.util")
  local user = util.require_if_exists("user")
  if not user then return {} end
  return user.session or {}
end

local function config_dir()
  local user = user_session_config()
  return user.directory or (vim.fn.stdpath("data") .. "/session")
end

local function autoread_enabled()
  local user = user_session_config()
  return user.autoread == true
end

local function autowrite_enabled()
  local user = user_session_config()
  if user.autowrite == nil then return true end
  return user.autowrite == true
end

local function current_cwd()
  return workspace.cwd()
end

local function project_root()
  return workspace.current_root(0)
end

local function ensure_session_dir()
  local dir = config_dir()
  if vim.fn.isdirectory(dir) ~= 1 then vim.fn.mkdir(dir, "p") end
  return dir
end

local function session_slug(path)
  local normalized = workspace.normalize(path or current_cwd())
  local drive = normalized:match("^([A-Za-z]):")
  if drive then
    normalized = drive:lower() .. normalized:sub(3)
  end

  local slug = normalized
    :gsub("^[/\\]+", "")
    :gsub("[:/\\]+", "__")
    :gsub("%s+", "_")
    :gsub("[^%w%._%-]+", "_")
    :gsub("_+", "_")
    :gsub("^_+", "")
    :gsub("_+$", "")

  if slug == "" then return "workspace.vim" end
  return slug .. ".vim"
end

local function session_name_for_root(root)
  return session_slug(root or project_root())
end

local function session_root_from_name(name)
  local bare = tostring(name or ""):gsub("%.vim$", "")
  if bare == "" then return nil end

  local root = bare:gsub("__", "/")
  if root:match("^[A-Za-z]/") then
    root = root:sub(1, 1):upper() .. ":" .. root:sub(2)
  elseif not root:match("^/") then
    root = "/" .. root
  end

  return workspace.normalize(root)
end

local function read_session_root_metadata(path)
  if vim.fn.filereadable(path) ~= 1 then return nil end

  local ok, lines = pcall(vim.fn.readfile, path, "", 5)
  if not ok or type(lines) ~= "table" then return nil end

  for _, line in ipairs(lines) do
    if vim.startswith(line, SESSION_ROOT_PREFIX) then
      return workspace.normalize(line:sub(#SESSION_ROOT_PREFIX + 1))
    end
  end

  return nil
end

local function write_session_root_metadata(name, root)
  local path = session_file_path(name)
  if vim.fn.filereadable(path) ~= 1 then return end

  local ok, lines = pcall(vim.fn.readfile, path)
  if not ok or type(lines) ~= "table" then return end

  local header = SESSION_ROOT_PREFIX .. root
  if lines[1] == header then return end

  if type(lines[1]) == "string" and vim.startswith(lines[1], SESSION_ROOT_PREFIX) then
    lines[1] = header
  else
    table.insert(lines, 1, header)
  end

  pcall(vim.fn.writefile, lines, path)
end

local function session_display_name(name)
  return tostring(name or ""):gsub("%.vim$", "")
end

local function active_session_name()
  local this_session = vim.v.this_session
  if type(this_session) ~= "string" or this_session == "" then return nil end
  return vim.fn.fnamemodify(this_session, ":t")
end

local function current_session_matches(root)
  return active_session_name() == session_name_for_root(root)
end

function session_file_path(name)
  return vim.fs.joinpath(ensure_session_dir(), name)
end

local function session_items()
  local items = {}
  local directory = ensure_session_dir()

  for name in vim.fs.dir(directory) do
    if name:sub(-4) == ".vim" then
      local path = vim.fs.joinpath(directory, name)
      if vim.fn.isdirectory(path) ~= 1 then
        local root = read_session_root_metadata(path) or session_root_from_name(name)
        table.insert(items, {
          name = name,
          path = path,
          root = root,
          modify_time = vim.fn.getftime(path),
        })
      end
    end
  end

  table.sort(items, function(a, b)
    if a.modify_time == b.modify_time then return a.name < b.name end
    return a.modify_time > b.modify_time
  end)

  return items
end

local function session_exists(name)
  return vim.uv.fs_stat(session_file_path(name)) ~= nil
end

local function session_label(item)
  local root = item.root
  local label = root and ("%s  %s"):format(vim.fs.basename(root), root) or item.name
  local prefix = active_session_name() == item.name and "* " or "  "
  return prefix .. label
end

local function restore_runtime_state()
  local ok_ui, ui = pcall(require, "plugins.ui")
  if ok_ui and ui and type(ui.apply_theme) == "function" then ui.apply_theme() end
end

local function session_error(err)
  local message = tostring(err or ""):gsub("^.-%(mini%.sessions%)%s*", "")
  if message == "" then message = "Session command failed." end
  return message
end

local function with_session_call(fn)
  local ok, result = pcall(fn)
  if ok then return result end
  vim.notify(session_error(result), vim.log.levels.WARN, { title = "Sessions" })
  return nil
end

local function should_autoread()
  if vim.fn.argc() > 0 then return false end

  local listed = vim.tbl_filter(function(bufnr)
    return vim.fn.buflisted(bufnr) == 1
  end, vim.api.nvim_list_bufs())
  if #listed > 1 then return false end

  if vim.bo.filetype ~= "" then return false end

  local line_count = vim.api.nvim_buf_line_count(0)
  if line_count > 1 then return false end
  local first_line = vim.api.nvim_buf_get_lines(0, 0, 1, true)[1] or ""
  if first_line ~= "" then return false end

  return true
end

local function ensure_loaded()
  local loader = require("pack.loader")
  if not loader.ensure("mini.nvim") then return nil end

  local ok, mini_sessions = pcall(require, "mini.sessions")
  if not ok then
    vim.notify("Failed to load `mini.sessions`.", vim.log.levels.ERROR, { title = "Sessions" })
    return nil
  end

  if not M._setup_done then
    mini_sessions.setup({
      autoread = false,
      autowrite = false,
      directory = ensure_session_dir(),
      file = "",
      force = {
        read = false,
        write = true,
        delete = false,
      },
      verbose = {
        read = false,
        write = false,
        delete = false,
      },
      hooks = {
        post = {
          read = restore_runtime_state,
        },
      },
    })
    M._setup_done = true
  end

  return mini_sessions
end

function M.current_name()
  return session_name_for_root(project_root())
end

function M.current_root()
  return project_root()
end

function M.name_for_root(root)
  return session_name_for_root(root)
end

function M.exists_for_root(root)
  return session_exists(session_name_for_root(root))
end

function M.items()
  return session_items()
end

local function save_session(name, root, notify_success)
  local mini_sessions = ensure_loaded()
  if not mini_sessions then return false end

  local ok = with_session_call(function()
    mini_sessions.write(name, { force = true, verbose = false })
    write_session_root_metadata(name, root)
    return true
  end)
  if ok and notify_success ~= false then
    vim.notify(("Saved workspace session: %s"):format(root), vim.log.levels.INFO, { title = "Sessions" })
  end
  return ok == true
end

local function load_session(name, root, notify_success)
  local mini_sessions = ensure_loaded()
  if not mini_sessions then return false end

  local ok = with_session_call(function()
    mini_sessions.read(name, { verbose = false })
    return true
  end)
  if ok and notify_success ~= false then
    vim.notify(("Loaded workspace session: %s"):format(root), vim.log.levels.INFO, { title = "Sessions" })
  end
  return ok == true
end

function M.save_for_root(root, notify_success)
  root = workspace.normalize(root or project_root())
  if not root then return false end
  return save_session(session_name_for_root(root), root, notify_success)
end

function M.load_for_root(root, notify_success)
  root = workspace.normalize(root or project_root())
  if not root then return false end

  local name = session_name_for_root(root)
  if not session_exists(name) then return false end
  return load_session(name, root, notify_success)
end

local function delete_session(name, root, opts, notify_success)
  local mini_sessions = ensure_loaded()
  if not mini_sessions then return false end

  local ok = with_session_call(function()
    mini_sessions.delete(name, vim.tbl_extend("force", opts or {}, { verbose = false }))
    return true
  end)
  if ok and notify_success ~= false then
    vim.notify(("Deleted workspace session: %s"):format(root), vim.log.levels.INFO, { title = "Sessions" })
  end
  return ok == true
end

function M.save_current()
  local root = project_root()
  M.save_for_root(root, true)
end

function M.load_current()
  local root = project_root()
  if not M.exists_for_root(root) then
    vim.notify(("No saved workspace session yet for: %s"):format(root), vim.log.levels.INFO, { title = "Sessions" })
    return
  end
  M.load_for_root(root, true)
end

function M.delete_current()
  local root = project_root()
  local name = session_name_for_root(root)
  if not M.exists_for_root(root) then
    vim.notify(("No saved workspace session to delete for: %s"):format(root), vim.log.levels.INFO, { title = "Sessions" })
    return
  end

  local opts = {}
  if current_session_matches(root) then opts.force = true end
  delete_session(name, root, opts, true)
end

function M.select(action)
  local items = session_items()
  if #items == 0 then
    vim.notify("No saved workspace sessions yet.", vim.log.levels.INFO, { title = "Sessions" })
    return
  end

  local session_action = action or "read"
  local prompts = {
    read = "Workspace sessions",
    delete = "Delete workspace session",
  }

  vim.ui.select(items, {
    prompt = prompts[session_action] or "Workspace sessions",
    format_item = session_label,
  }, function(choice)
    if not choice then return end

    if session_action == "delete" then
      local opts = {}
      if active_session_name() == choice.name then opts.force = true end
      delete_session(choice.name, choice.root or choice.name, opts, true)
      return
    end

    load_session(choice.name, choice.root or choice.name, true)
  end)
end

function M.restart()
  local mini_sessions = ensure_loaded()
  if not mini_sessions then return end

  if vim.v.this_session == "" then M.save_current() end
  with_session_call(function()
    mini_sessions.restart()
    return true
  end)
end

function M.show_current()
  local root = project_root()
  local name = session_name_for_root(root)
  local lines = {
    "Workspace session",
    "",
    "Root:    " .. root,
    "Name:    " .. session_display_name(name),
    "Path:    " .. session_file_path(name),
    "Active:  " .. (current_session_matches(root) and "yes" or "no"),
    "Auto:    " .. ("read=%s, write=%s"):format(autoread_enabled() and "on" or "off", autowrite_enabled() and "on" or "off"),
  }

  vim.notify(table.concat(lines, "\n"), vim.log.levels.INFO, { title = "Sessions" })
end

function M.setup()
  if M._did_setup then return end
  M._did_setup = true

  local group = vim.api.nvim_create_augroup("core_workspace_sessions", { clear = true })

  if autoread_enabled() then
    vim.api.nvim_create_autocmd("VimEnter", {
      group = group,
      once = true,
      nested = true,
      callback = function()
        if not should_autoread() then return end

        local root = project_root()
        local name = session_name_for_root(root)
        if not session_exists(name) then return end
        load_session(name, root, false)
      end,
      desc = "Autoread current workspace session",
    })
  end

  if autowrite_enabled() then
    vim.api.nvim_create_autocmd("VimLeavePre", {
      group = group,
      callback = function()
        if vim.v.this_session == "" then return end
        local mini_sessions = ensure_loaded()
        if not mini_sessions then return end
        with_session_call(function()
          mini_sessions.write(nil, { force = true, verbose = false })
          return true
        end)
      end,
      desc = "Autowrite active workspace session",
    })
  end
end

return M
