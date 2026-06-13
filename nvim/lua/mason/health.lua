local health = vim.health or require("health")

local start = health.start or health.report_start
local ok = health.ok or health.report_ok
local warn = health.warn or health.report_warn
local err = health.error or health.report_error
local info = health.info or health.report_info

local M = {}

function M.check()
  start("mason.nvim (local health shim)")

  local loaded, mason = pcall(require, "mason")
  if loaded and mason then
    ok("mason.nvim can be required")
  else
    err("mason.nvim failed to load: " .. tostring(mason))
    return
  end

  local mason_dir = vim.fs.joinpath(vim.fn.stdpath("data"), "mason")
  if vim.fn.isdirectory(mason_dir) == 1 then
    ok("Mason data directory exists: " .. mason_dir)
  else
    warn("Mason data directory does not exist yet: " .. mason_dir)
  end

  local log_dir = vim.fs.joinpath(vim.fn.stdpath("data"), "logs")
  local log_file = vim.fs.joinpath(log_dir, "mason.log")
  local writable = vim.fn.filewritable(log_dir)
  if writable == 2 then
    ok("Mason log directory is writable: " .. log_dir)
  else
    warn("Mason log directory is not writable: " .. log_dir)
  end

  if vim.fn.filereadable(log_file) == 1 then
    info("Mason log file: " .. log_file)
  else
    info("Mason log file does not exist yet: " .. log_file)
  end

  if vim.fn.executable("git") == 1 then
    ok("`git` is available on PATH")
  else
    warn("`git` is not available on PATH")
  end

  warn("Using local Mason health shim to avoid the upstream Windows log-permission hang during :checkhealth.")
end

return M
