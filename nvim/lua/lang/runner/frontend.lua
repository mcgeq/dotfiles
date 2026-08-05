local M = {}

local util = require "core.util"
local shell_quote = util.shell_quote

local exec = require "lang.runner.exec"
local detect = require "lang.runner.detect"

local function js_test_errorformat() return "%E%f:%l:%c: %m,%E%f:%l: %m,%W%f:%l:%c: %m,%W%f:%l: %m,%-G%.%#" end

local function script_command(bufnr, action)
  local root = detect.package_json_root(bufnr)
  if not root then return nil, "Frontend runner requires a nearby package.json." end

  local data = detect.package_json_data(root)
  local scripts = data and data.scripts or {}
  if action == "clean" and (type(scripts) ~= "table" or type(scripts.clean) ~= "string" or scripts.clean == "") then
    return nil, "No `clean` script found in package.json."
  end
  if type(scripts) ~= "table" or type(scripts[action]) ~= "string" or scripts[action] == "" then
    return nil, ("No `%s` script found in package.json."):format(action)
  end

  local manager = detect.detect_package_manager(root)
  if not manager then return nil, "No supported package manager found on PATH. Expected pnpm, bun, yarn, or npm." end

  return exec.shell_command(detect.package_manager_command(manager, action), root, " Frontend ")
end

local function test_file_command(bufnr)
  local root = detect.package_json_root(bufnr)
  if not root then return nil, "Frontend test runner requires a nearby package.json." end

  local data = detect.package_json_data(root)
  local scripts = data and data.scripts or {}
  if type(scripts) ~= "table" or type(scripts.test) ~= "string" or scripts.test == "" then
    return nil, "No `test` script found in package.json."
  end

  local manager = detect.detect_package_manager(root)
  if not manager then return nil, "No supported package manager found on PATH. Expected pnpm, bun, yarn, or npm." end

  local file = vim.fn.fnamemodify(vim.api.nvim_buf_get_name(bufnr), ":p")
  local test_script = scripts.test
  local command
  if test_script:match "vitest" then
    command = detect.package_manager_command(manager, "test") .. " -- " .. shell_quote(file)
  elseif test_script:match "jest" then
    command = detect.package_manager_command(manager, "test") .. " -- " .. shell_quote(file)
  else
    return nil, "Current `test` script is not recognized as a file-filterable runner."
  end

  return exec.quickfix_command(command, root, " Frontend Tests ", js_test_errorformat())
end

function M.command(bufnr, action)
  if action == "test_file" then return test_file_command(bufnr) end
  return script_command(bufnr, action)
end

return M
