local M = {}

local util = require "core.util"
local executable_suffix = util.executable_suffix
local shell_quote = util.shell_quote
local powershell_quote = util.powershell_quote
local first_executable = util.first_executable
local is_windows = util.is_windows

local exec = require "lang.runner.exec"
local detect = require "lang.runner.detect"
local file_exists = detect.file_exists

local function single_file_command(bufnr, action)
  local file = vim.fn.fnamemodify(vim.api.nvim_buf_get_name(bufnr), ":p")
  local dir = vim.fn.fnamemodify(file, ":h")
  local stem = vim.fn.fnamemodify(file, ":t:r")
  local output = vim.fs.joinpath(dir, stem .. executable_suffix())
  local test_output = vim.fs.joinpath(dir, stem .. "_test" .. executable_suffix())
  local rustc = first_executable { "rustc" }
  if not rustc then return nil, "No `rustc` executable found on PATH." end

  if action == "clean" then return {
    clean = true,
    paths = { output, test_output },
    title = "RUST",
  } end

  local pieces = { shell_quote(rustc) }
  if action == "build" then
    pieces[#pieces + 1] = shell_quote(file)
    pieces[#pieces + 1] = "-o"
    pieces[#pieces + 1] = shell_quote(output)
  elseif action == "run" then
    pieces[#pieces + 1] = shell_quote(file)
    pieces[#pieces + 1] = "-o"
    pieces[#pieces + 1] = shell_quote(output)
    if is_windows() then
      pieces[#pieces + 1] = ";"
      pieces[#pieces + 1] = "if ($LASTEXITCODE -eq 0) { & " .. powershell_quote(output) .. " }"
    else
      pieces[#pieces + 1] = "&&"
      pieces[#pieces + 1] = shell_quote(output)
    end
  elseif action == "test" then
    pieces[#pieces + 1] = "--test"
    pieces[#pieces + 1] = shell_quote(file)
    pieces[#pieces + 1] = "-o"
    pieces[#pieces + 1] = shell_quote(test_output)
    if is_windows() then
      pieces[#pieces + 1] = ";"
      pieces[#pieces + 1] = "if ($LASTEXITCODE -eq 0) { & " .. powershell_quote(test_output) .. " }"
    else
      pieces[#pieces + 1] = "&&"
      pieces[#pieces + 1] = shell_quote(test_output)
    end
  end

  return exec.shell_command(table.concat(pieces, " "), dir, " Rust ")
end

function M.command(bufnr, action)
  local root = detect.is_rust_project(bufnr)
  if root and file_exists(vim.fs.joinpath(root, "Cargo.toml")) then
    local title = " Rust "
    if action == "build" then
      return exec.shell_command("cargo build", root, title)
    elseif action == "run" then
      return exec.shell_command("cargo run", root, title)
    elseif action == "test" then
      return exec.shell_command("cargo test", root, title)
    elseif action == "clean" then
      return exec.shell_command("cargo clean", root, title)
    end
  end

  return single_file_command(bufnr, action)
end

return M
