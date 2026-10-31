local M = {}
local util = require "core.util"

function M.resolve(path)
  if not path or path == "" then return nil end
  local root = vim.fs.root(path, { "pyproject.toml", "uv.lock", "requirements.txt", "ruff.toml", ".ruff.toml", ".git" })
  if not root then return nil end
  for _, name in ipairs { ".venv", "venv" } do
    local venv = vim.fs.joinpath(root, name)
    local bin = vim.fs.joinpath(venv, util.is_windows() and "Scripts" or "bin")
    local python = vim.fs.joinpath(bin, "python" .. util.executable_suffix())
    if vim.fn.filereadable(python) == 1 then
      return {
        root = root,
        venv_dir = venv,
        bin_dir = bin,
        python = vim.fs.normalize(python),
        env = {
          VIRTUAL_ENV = venv,
          PATH = bin .. (util.is_windows() and ";" or ":") .. (vim.env.PATH or ""),
        },
      }
    end
  end
end

function M.for_buffer(bufnr) return M.resolve(vim.api.nvim_buf_get_name(bufnr)) end

function M.executable(name, path)
  local project = M.resolve(path)
  if project then
    local candidate = vim.fs.joinpath(project.bin_dir, name .. util.executable_suffix())
    if vim.fn.executable(candidate) == 1 then return candidate end
  end
  return name
end

function M.ty_command(path)
  local executable = M.executable("ty", path)
  if vim.fn.executable(executable) == 1 then return { executable, "server" } end
end

function M.checker(path) return M.ty_command(path) and "ty" or "basedpyright" end

function M.checker_root(name)
  return function(bufnr, on_dir)
    local file = vim.api.nvim_buf_get_name(bufnr)
    local root = vim.fs.root(bufnr, require("lsp.servers.common").PYTHON_ROOT_MARKERS) or vim.fs.dirname(file)
    if root and M.checker(root) == name then on_dir(root) end
  end
end

function M.lsp_command(command)
  return function(dispatchers, config)
    local project = M.resolve(config.root_dir)
    local cmd = vim.deepcopy(command)
    cmd[1] = M.executable(cmd[1], config.root_dir)
    return vim.lsp.rpc.start(cmd, dispatchers, {
      cwd = config.root_dir,
      env = project and project.env or nil,
    })
  end
end

function M.before_init(_, config)
  local project = M.resolve(config.root_dir)
  if not project then return end
  -- LSP clients keep a reference to this table before before_init runs.
  config.settings.python = vim.tbl_deep_extend("force", config.settings.python or {}, { pythonPath = project.python })
  config.settings.ty = vim.tbl_deep_extend("force", config.settings.ty or {}, {
    configuration = { environment = { python = project.python } },
  })
end

return M
