local M = {}

local util = require "core.util"
local executable_suffix = util.executable_suffix
local shell_quote = util.shell_quote
local first_executable = util.first_executable

local exec = require "lang.runner.exec"
local detect = require "lang.runner.detect"

local function single_file_command(bufnr, action)
  local file = vim.fn.fnamemodify(vim.api.nvim_buf_get_name(bufnr), ":p")
  local dir = vim.fn.fnamemodify(file, ":h")
  local stem = vim.fn.fnamemodify(file, ":t:r")
  local output = vim.fs.joinpath(dir, stem .. executable_suffix())
  local zig = first_executable { "zig" }
  if not zig then return nil, "No `zig` executable found on PATH." end

  if action == "clean" then return {
    clean = true,
    paths = { output },
    title = "ZIG",
  } end

  local pieces = { shell_quote(zig) }
  if action == "build" then
    pieces[#pieces + 1] = "build-exe"
    pieces[#pieces + 1] = shell_quote(file)
  elseif action == "run" then
    pieces[#pieces + 1] = "run"
    pieces[#pieces + 1] = shell_quote(file)
  elseif action == "test" then
    pieces[#pieces + 1] = "test"
    pieces[#pieces + 1] = shell_quote(file)
  else
    return nil, "Single-file Zig clean is not configured."
  end

  return exec.shell_command(table.concat(pieces, " "), dir, " Zig ")
end

function M.command(bufnr, action)
  local root = detect.is_zig_project(bufnr)
  if root and vim.fn.filereadable(vim.fs.joinpath(root, "build.zig")) == 1 then
    if action == "build" then
      return exec.shell_command("zig build", root, " Zig ")
    elseif action == "run" then
      return exec.shell_command("zig build run", root, " Zig ")
    elseif action == "test" then
      return exec.shell_command("zig build test", root, " Zig ")
    elseif action == "clean" then
      return nil, "Zig project clean is not configured. `zig build` has no universal clean command."
    end
  end

  return single_file_command(bufnr, action)
end

return M
