local M = {}

local function print_lines(lines)
  vim.api.nvim_out_write(table.concat(lines, "\n"))
  vim.api.nvim_out_write("\n")
end

local function is_nvim_update_check(cmd)
  return type(cmd) == "table"
    and cmd[1] == "git"
    and cmd[2] == "ls-remote"
    and cmd[3] == "https://github.com/neovim/neovim"
end

local function skipped_system_result()
  return {
    code = 1,
    signal = 0,
    stdout = "",
    stderr = "Skipped Neovim update check in background health capture",
  }
end

local function skipped_system_obj()
  return {
    wait = function()
      return skipped_system_result()
    end,
    kill = function() end,
    write = function() end,
    is_closing = function()
      return true
    end,
  }
end

local function prepare_headless_snacks_health()
  if #vim.api.nvim_list_uis() ~= 0 then return end

  local ok_snacks, snacks = pcall(require, "snacks")
  if not ok_snacks or not snacks.did_setup then return end
  if not (snacks.config.dashboard and snacks.config.dashboard.enabled) then return end

  local ok_dashboard, dashboard = pcall(require, "snacks.dashboard")
  if not ok_dashboard or type(dashboard.setup) ~= "function" then return end
  if dashboard.status and dashboard.status.did_setup then return end

  -- `Snacks.dashboard` normally initializes on `UIEnter`, which never fires in
  -- our background headless health process. Run its setup once so health can
  -- report the real headless reason instead of a false "setup did not run".
  pcall(dashboard.setup)
end

function M.run()
  local old_echo = vim.api.nvim_echo
  local old_system = vim.system
  vim.api.nvim_echo = function() return 0 end
  vim.system = function(cmd, opts, on_exit)
    if is_nvim_update_check(cmd) then
      local result = skipped_system_result()
      if on_exit then
        vim.schedule(function()
          on_exit(result)
        end)
      end
      return skipped_system_obj()
    end
    return old_system(cmd, opts, on_exit)
  end

  prepare_headless_snacks_health()

  local ok, err = pcall(function()
    require("vim.health")._check("", vim.env.NVIM_HEALTH_ARGS or "")
  end)

  vim.api.nvim_echo = old_echo
  vim.system = old_system

  if not ok then
    vim.api.nvim_err_writeln("Health capture failed: " .. tostring(err))
    vim.cmd.cquit(1)
    return
  end

  local bufnr = vim.fn.bufnr("health://")
  if bufnr < 0 then
    vim.api.nvim_err_writeln("Health capture failed: health:// buffer was not created")
    vim.cmd.cquit(1)
    return
  end

  print_lines(vim.api.nvim_buf_get_lines(bufnr, 0, -1, false))
end

return M
