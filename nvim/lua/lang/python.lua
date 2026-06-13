local M = {}

local ROOT_MARKERS = { "pyproject.toml", "uv.lock", "requirements.txt", ".git" }
local PROJECT_MARKERS = { "pyproject.toml", "uv.lock", "requirements.txt", "ruff.toml", ".ruff.toml" }
local VENV_NAMES = { ".venv", "venv" }
local activated_roots = {}

local util = require("core.util")
local is_windows = util.is_windows

local function joinpath(...)
  return vim.fs.joinpath(...)
end

local function project_root(path)
  if not path or path == "" then return nil end
  return vim.fs.root(path, ROOT_MARKERS)
end

local function looks_like_python_root(root)
  if not root or root == "" then return false end

  for _, marker in ipairs(PROJECT_MARKERS) do
    if vim.fn.filereadable(joinpath(root, marker)) == 1 then return true end
  end

  for _, name in ipairs(VENV_NAMES) do
    if vim.fn.isdirectory(joinpath(root, name)) == 1 then return true end
  end

  return false
end

local function activate_root(root, opts)
  if not root or root == "" then return nil end

  opts = opts or {}
  local win = is_windows()
  local path_sep = win and ";" or ":"

  for _, name in ipairs(VENV_NAMES) do
    local venv_dir = joinpath(root, name)
    if vim.fn.isdirectory(venv_dir) == 1 then
      local bin_dir = joinpath(venv_dir, win and "Scripts" or "bin")
      local python = joinpath(bin_dir, win and "python.exe" or "python")
      if vim.fn.filereadable(python) == 1 then
        local current_path = vim.env.PATH or ""
        local normalized_bin = win and bin_dir:lower() or bin_dir
        local normalized_path = win and current_path:lower() or current_path
        if vim.fn.isdirectory(bin_dir) == 1 and not normalized_path:find(normalized_bin, 1, true) then
          vim.env.PATH = bin_dir .. path_sep .. current_path
        end

        vim.env.VIRTUAL_ENV = venv_dir
        vim.g.python3_host_prog = python
        if opts.bufnr and vim.api.nvim_buf_is_valid(opts.bufnr) then
          vim.b[opts.bufnr].python3_host_prog = python
        end

        if opts.notify and activated_roots[root] ~= python then
          vim.notify("Python virtualenv detected: " .. name, vim.log.levels.INFO, { title = "Python" })
        end

        activated_roots[root] = python
        return {
          root = root,
          venv_dir = venv_dir,
          bin_dir = bin_dir,
          python = python,
        }
      end
    end
  end
end

function M.activate_for_path(path, opts)
  local root = project_root(path)
  if not root then return nil end
  if opts and opts.require_python_root and not looks_like_python_root(root) then return nil end
  return activate_root(root, opts)
end

function M.activate_for_buffer(bufnr, opts)
  if not bufnr or not vim.api.nvim_buf_is_valid(bufnr) then return nil end

  local file = vim.api.nvim_buf_get_name(bufnr)
  if file == "" then return nil end

  opts = vim.tbl_extend("force", opts or {}, { bufnr = bufnr })
  return M.activate_for_path(file, opts)
end

function M.prime()
  local current = M.activate_for_buffer(vim.api.nvim_get_current_buf(), { notify = false, require_python_root = true })
  if current then return current end

  local cwd = vim.uv.cwd()
  local from_cwd = M.activate_for_path(cwd, { notify = false, require_python_root = true })
  if from_cwd then return from_cwd end

  for _, bufnr in ipairs(vim.api.nvim_list_bufs()) do
    local result = M.activate_for_buffer(bufnr, { notify = false, require_python_root = true })
    if result then return result end
  end
end

function M.setup()
  vim.api.nvim_create_autocmd("FileType", {
    pattern = "python",
    group = vim.api.nvim_create_augroup("lang_python_venv", { clear = true }),
    callback = function(event)
      if vim.b[event.buf].venv_checked then return end
      vim.b[event.buf].venv_checked = true
      M.activate_for_buffer(event.buf, { notify = true })
    end,
  })
end

return M
