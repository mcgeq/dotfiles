local M = {}

M.PROJECT_ROOT_MARKERS = {
  "pnpm-workspace.yaml",
  "package.json",
  "tsconfig.json",
  "jsconfig.json",
  "pyproject.toml",
  "uv.lock",
  "requirements.txt",
  "Cargo.toml",
  "rust-project.json",
  "go.work",
  "go.mod",
  "build.zig",
  "zls.json",
  "CMakeLists.txt",
  "Makefile",
  ".git",
}

local function current_cwd_raw()
  return vim.uv.cwd() or vim.fn.getcwd()
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

function M.normalize(path)
  if type(path) ~= "string" or path == "" then return nil end
  return vim.fs.normalize(path)
end

function M.cwd()
  return M.normalize(current_cwd_raw()) or vim.fs.normalize(vim.fn.getcwd())
end

function M.dir_label(path)
  local normalized = M.normalize(path) or M.cwd()
  local name = vim.fs.basename(normalized)
  if name ~= "" then return name end
  return normalized
end

function M.same_path(a, b)
  local left = M.normalize(a)
  local right = M.normalize(b)
  return left ~= nil and left == right
end

function M.buffer_path(bufnr)
  bufnr = bufnr or vim.api.nvim_get_current_buf()
  local name = vim.api.nvim_buf_get_name(bufnr)
  if name == "" then return nil end
  return M.normalize(name)
end

function M.buffer_dir(bufnr)
  local path = M.buffer_path(bufnr)
  if not path then return nil end
  return M.normalize(vim.fn.fnamemodify(path, ":p:h"))
end

function M.project_root(target)
  target = target or vim.api.nvim_get_current_buf()
  local root = vim.fs.root(target, M.PROJECT_ROOT_MARKERS)
  if root then return M.normalize(root) end
  return nil
end

function M.cwd_project_root()
  local cwd = M.cwd()
  local root = vim.fs.root(cwd, M.PROJECT_ROOT_MARKERS)
  if root then return M.normalize(root) end
  return nil
end

function M.current_root(bufnr)
  return M.project_root(bufnr) or M.buffer_dir(bufnr) or M.cwd()
end

function M.buffer_roots()
  local roots = {}
  local seen = {}

  for _, bufnr in ipairs(listed_buffers()) do
    local root = M.current_root(bufnr)
    if root and not seen[root] then
      seen[root] = true
      table.insert(roots, root)
    end
  end

  table.sort(roots)
  return roots
end

function M.info(bufnr)
  local cwd = M.cwd()
  local cwd_root = M.cwd_project_root()
  local buffer_dir = M.buffer_dir(bufnr)
  local buffer_root = M.project_root(bufnr)

  return {
    cwd = cwd,
    cwd_root = cwd_root or cwd,
    buffer_dir = buffer_dir,
    buffer_root = buffer_root,
    current_root = M.current_root(bufnr),
  }
end

local function merge_project(items, index, root, extra)
  root = M.normalize(root)
  if not root then return end

  local item = index[root]
  if not item then
    item = {
      root = root,
      name = M.dir_label(root),
      current = false,
      cwd = false,
      open = false,
      has_session = false,
      modify_time = nil,
    }
    index[root] = item
    table.insert(items, item)
  end

  extra = extra or {}
  item.current = item.current or extra.current == true
  item.cwd = item.cwd or extra.cwd == true
  item.open = item.open or extra.open == true
  item.has_session = item.has_session or extra.has_session == true

  if type(extra.modify_time) == "number" then
    if type(item.modify_time) ~= "number" or extra.modify_time > item.modify_time then
      item.modify_time = extra.modify_time
    end
  end
end

function M.known_projects()
  local items = {}
  local index = {}
  local info = M.info(0)

  merge_project(items, index, info.current_root, { current = true, open = true })
  merge_project(items, index, info.cwd_root, { cwd = true })

  for _, root in ipairs(M.buffer_roots()) do
    merge_project(items, index, root, { open = true })
  end

  local ok, sessions = pcall(require, "core.sessions")
  if ok and sessions and type(sessions.items) == "function" then
    for _, session in ipairs(sessions.items()) do
      if session.root then
        merge_project(items, index, session.root, {
          has_session = true,
          modify_time = session.modify_time,
        })
      end
    end
  end

  table.sort(items, function(a, b)
    if a.current ~= b.current then return a.current end
    if a.cwd ~= b.cwd then return a.cwd end
    if a.has_session ~= b.has_session then return a.has_session end

    local a_time = type(a.modify_time) == "number" and a.modify_time or -1
    local b_time = type(b.modify_time) == "number" and b.modify_time or -1
    if a_time ~= b_time then return a_time > b_time end

    if a.name ~= b.name then return a.name < b.name end
    return a.root < b.root
  end)

  return items
end

function M.project_label(item)
  local tags = {}

  if item.current then tags[#tags + 1] = "current" end
  if item.cwd then tags[#tags + 1] = "cwd" end
  if item.has_session then tags[#tags + 1] = "session" end
  if item.open then tags[#tags + 1] = "open" end

  local suffix = #tags > 0 and ("  [" .. table.concat(tags, ", ") .. "]") or ""
  return ("%s  %s%s"):format(M.dir_label(item.root), item.root, suffix)
end

function M.cd(root, opts)
  opts = opts or {}
  local target = M.normalize(root)
  if not target then
    vim.notify("No workspace root available to switch to.", vim.log.levels.WARN, { title = "Projects" })
    return false
  end

  if not M.same_path(target, M.cwd()) then
    vim.cmd.cd(vim.fn.fnameescape(target))
  end

  local loaded_session = false
  local has_session = false

  if opts.load_session then
    local ok, sessions = pcall(require, "core.sessions")
    if ok and sessions then
      if type(sessions.exists_for_root) == "function" and sessions.exists_for_root(target) then
        has_session = true
        if type(sessions.load_for_root) == "function" then
          loaded_session = sessions.load_for_root(target, true) == true
        end
      end
    end
  end

  if not loaded_session and opts.notify ~= false then
    vim.notify(("Project root -> %s"):format(target), vim.log.levels.INFO, { title = "Projects" })
  end

  if opts.load_session and not has_session and opts.notify_missing_session ~= false then
    vim.notify(("No saved workspace session yet for: %s"):format(target), vim.log.levels.INFO, { title = "Projects" })
  end

  return true
end

function M.cd_to_current_root(opts)
  local target = M.project_root(0) or M.buffer_dir(0) or M.cwd()
  return M.cd(target, opts)
end

function M.select(opts)
  opts = opts or {}
  local items = M.known_projects()
  if #items == 0 then
    vim.notify("No known projects yet. Save a workspace session or open a project buffer first.", vim.log.levels.INFO, {
      title = "Projects",
    })
    return
  end

  vim.ui.select(items, {
    prompt = opts.load_session and "Projects + sessions" or "Projects",
    format_item = M.project_label,
  }, function(choice)
    if not choice then return end
    M.cd(choice.root, opts)
  end)
end

function M.show_info()
  local info = M.info(0)
  local lines = {
    "Workspace root",
    "",
    "Cwd:         " .. info.cwd,
    "Cwd root:    " .. info.cwd_root,
    "Buffer dir:  " .. (info.buffer_dir or "n/a"),
    "Buffer root: " .. (info.buffer_root or "n/a"),
    "Active root: " .. info.current_root,
  }

  local ok, sessions = pcall(require, "core.sessions")
  if ok and sessions then
    local has_session = type(sessions.exists_for_root) == "function" and sessions.exists_for_root(info.current_root)
    lines[#lines + 1] = "Session:     " .. (has_session and "saved" or "none")
  end

  vim.notify(table.concat(lines, "\n"), vim.log.levels.INFO, { title = "Projects" })
end

return M
