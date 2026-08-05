local M = {}

local util = require "core.util"
local shell_quote = util.shell_quote

local exec = require "lang.runner.exec"
local detect = require "lang.runner.detect"

function M.command(bufnr, action)
  local root = detect.is_go_project(bufnr)
  local file = vim.fn.fnamemodify(vim.api.nvim_buf_get_name(bufnr), ":p")
  local dir = vim.fn.fnamemodify(file, ":h")

  if action == "build" then
    if root and vim.fn.filereadable(vim.fs.joinpath(root, "go.mod")) == 1 then
      return exec.shell_command("go build .", dir, " Go ")
    end
    return exec.shell_command("go build " .. shell_quote(file), dir, " Go ")
  elseif action == "run" then
    if root and vim.fn.filereadable(vim.fs.joinpath(root, "go.mod")) == 1 then
      return exec.shell_command("go run .", dir, " Go ")
    end
    return exec.shell_command("go run " .. shell_quote(file), dir, " Go ")
  elseif action == "test" then
    return exec.shell_command("go test", dir, " Go ")
  elseif action == "clean" then
    if root and vim.fn.filereadable(vim.fs.joinpath(root, "go.mod")) == 1 then
      return exec.shell_command("go clean", dir, " Go ")
    end
    return nil, "Single-file Go clean is not configured."
  end
end

return M
