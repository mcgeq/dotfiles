local M = {}

local util = require "core.util"
local shell_quote = util.shell_quote
local first_executable = util.first_executable

local exec = require "lang.runner.exec"
local detect = require "lang.runner.detect"
local file_exists = detect.file_exists

local function python_errorformat() return "%E%f:%l: in %m,%E%f:%l:%c: %m,%E%f:%l: %m,%W%f:%l: %m,%-G%.%#" end

function M.command(bufnr, action)
  local root = detect.is_python_project(bufnr)
  local file = vim.fn.fnamemodify(vim.api.nvim_buf_get_name(bufnr), ":p")
  local dir = vim.fn.fnamemodify(file, ":h")
  local cwd = root or dir
  local uv_lock = root and file_exists(vim.fs.joinpath(root, "uv.lock"))
  local local_python = detect.find_local_python(root or dir)
  local python = local_python or first_executable { "python", "python3" }
  local cache_dir = vim.fs.joinpath(dir, "__pycache__")

  if action == "clean" then return {
    clean = true,
    paths = { cache_dir },
    title = "PYTHON",
  } end

  if uv_lock and vim.fn.executable "uv" == 1 then
    if action == "build" then
      return exec.shell_command("uv run python -m py_compile " .. shell_quote(file), cwd, " Python ")
    elseif action == "run" then
      return exec.shell_command("uv run python " .. shell_quote(file), cwd, " Python ")
    elseif action == "test" then
      return exec.shell_command("uv run pytest", cwd, " Python ")
    elseif action == "test_file" then
      return exec.quickfix_command("uv run pytest " .. shell_quote(file), cwd, " Python Tests ", python_errorformat())
    end
  end

  if not python then return nil, "No Python executable found on PATH." end

  local quoted_python = shell_quote(python)
  if action == "build" then
    return exec.shell_command(quoted_python .. " -m py_compile " .. shell_quote(file), cwd, " Python ")
  elseif action == "run" then
    return exec.shell_command(quoted_python .. " " .. shell_quote(file), cwd, " Python ")
  elseif action == "test" then
    return exec.shell_command(quoted_python .. " -m pytest", cwd, " Python ")
  elseif action == "test_file" then
    return exec.quickfix_command(
      quoted_python .. " -m pytest " .. shell_quote(file),
      cwd,
      " Python Tests ",
      python_errorformat()
    )
  end
end

return M
