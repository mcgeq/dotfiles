local M = {}

local util = require "core.util"
local is_windows = util.is_windows
local detect = require "lang.runner.detect"
local file_exists = detect.file_exists

function M.ensure_file_is_saved(bufnr, title)
  if vim.api.nvim_buf_get_name(bufnr) == "" then
    vim.notify("Current buffer has no file path yet.", vim.log.levels.WARN, { title = title })
    return false
  end

  if vim.bo[bufnr].modified then
    local ok, err = pcall(vim.cmd.write)
    if not ok then
      vim.notify(
        "Failed to save file before running command: " .. tostring(err),
        vim.log.levels.ERROR,
        { title = title }
      )
      return false
    end
  end

  return true
end

function M.open_command(cmd, opts) require("core.terminal").open_float(cmd, opts) end

function M.quickfix_command(command, cwd, title, errorformat)
  if is_windows() then
    return {
      cmd = {
        "powershell.exe",
        "-NoLogo",
        "-NoProfile",
        "-ExecutionPolicy",
        "Bypass",
        "-Command",
        command,
      },
      opts = {
        cwd = cwd,
        title = title,
        quickfix = true,
        errorformat = errorformat,
      },
    }
  end

  local shell = vim.o.shell ~= "" and vim.o.shell or "sh"
  local shellcmdflag = vim.o.shellcmdflag ~= "" and vim.o.shellcmdflag or "-c"
  return {
    cmd = {
      shell,
      shellcmdflag,
      command,
    },
    opts = {
      cwd = cwd,
      title = title,
      quickfix = true,
      errorformat = errorformat,
    },
  }
end

function M.shell_command(command, cwd, title)
  if is_windows() then
    return {
      cmd = {
        "powershell.exe",
        "-NoLogo",
        "-NoProfile",
        "-ExecutionPolicy",
        "Bypass",
        "-Command",
        command,
      },
      opts = {
        cwd = cwd,
        title = title,
      },
    }
  end

  local shell = vim.o.shell ~= "" and vim.o.shell or "sh"
  local shellcmdflag = vim.o.shellcmdflag ~= "" and vim.o.shellcmdflag or "-c"
  return {
    cmd = {
      shell,
      shellcmdflag,
      command,
    },
    opts = {
      cwd = cwd,
      title = title,
    },
  }
end

function M.remove_files(paths, title)
  local removed = {}
  for _, path in ipairs(paths) do
    if file_exists(path) then
      local kind = vim.fn.isdirectory(path) == 1 and "rf" or nil
      local ok, err = pcall(vim.fn.delete, path, kind)
      if not ok or file_exists(path) then
        vim.notify("Failed to remove " .. path .. ": " .. tostring(err), vim.log.levels.ERROR, { title = title })
        return
      end
      table.insert(removed, path)
    end
  end

  if #removed > 0 then
    vim.notify("Removed " .. table.concat(removed, ", "), vim.log.levels.INFO, { title = title })
  else
    vim.notify("Nothing to clean.", vim.log.levels.INFO, { title = title })
  end
end

function M.open_quickfix_result(result)
  local output = {}
  if result.stdout and result.stdout ~= "" then
    vim.list_extend(output, vim.split(result.stdout, "\n", { plain = true }))
  end
  if result.stderr and result.stderr ~= "" then
    vim.list_extend(output, vim.split(result.stderr, "\n", { plain = true }))
  end
  while #output > 0 and output[#output] == "" do
    table.remove(output)
  end

  local efm = result.opts and result.opts.errorformat or nil
  if efm and #output > 0 then vim.bo.errorformat = efm end
  vim.fn.setqflist({}, " ", {
    title = result.opts.title or "Test Output",
    lines = output,
    efm = efm,
  })
  vim.cmd.copen()

  local level = result.code == 0 and vim.log.levels.INFO or vim.log.levels.WARN
  local summary = result.code == 0 and "Command finished successfully."
    or ("Command exited with code " .. tostring(result.code))
  vim.notify(summary, level, { title = result.opts.title or "Runner" })
end

function M.execute_result(result)
  if result.quickfix then
    vim.system(result.cmd, { text = true, cwd = result.opts.cwd }, function(run_result)
      vim.schedule(
        function()
          M.open_quickfix_result {
            stdout = run_result.stdout,
            stderr = run_result.stderr,
            code = run_result.code,
            opts = result.opts,
          }
        end
      )
    end)
    return
  end

  M.open_command(result.cmd, result.opts)
end

return M
