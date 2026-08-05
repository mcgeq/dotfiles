local M = {}

local function current_init_path()
  local init_path = vim.env.MYVIMRC
  if type(init_path) == "string" and init_path ~= "" and vim.fn.filereadable(init_path) == 1 then return init_path end
  return vim.fs.joinpath(vim.fn.stdpath "config", "init.lua")
end

local function combine_process_output(result)
  local parts = {}
  if result.stdout and result.stdout ~= "" then table.insert(parts, result.stdout) end
  if result.stderr and result.stderr ~= "" then table.insert(parts, result.stderr) end
  return table.concat(parts, "\n")
end

function M.setup(open_scratch_window)
  vim.api.nvim_create_user_command("Health", function(opts)
    local command = opts.args ~= "" and ("checkhealth " .. opts.args) or "checkhealth"
    local init_path = current_init_path()

    local argv = { vim.v.progpath, "--headless", "-i", "NONE" }
    if vim.fn.filereadable(init_path) == 1 then vim.list_extend(argv, { "-u", init_path }) end
    vim.list_extend(argv, { "-c", "lua require('core.health_capture').run()", "-c", "qa" })

    local env = vim.fn.environ()
    env.NVIM_HEALTH_ARGS = opts.args
    if vim.env.NVIM_APPNAME and vim.env.NVIM_APPNAME ~= "" then env.NVIM_APPNAME = vim.env.NVIM_APPNAME end

    vim.notify("Health check started in background: " .. command, vim.log.levels.INFO, {
      title = "Health",
    })

    vim.system(argv, { text = true, cwd = vim.uv.cwd(), env = env }, function(result)
      vim.schedule(function()
        local output = combine_process_output(result)
        if output == "" then output = "(no output)" end
        local normalized = output:gsub("\r\n", "\n")
        local lines = {
          "Health check finished",
          "",
          "Command: " .. command,
          "Exit:    " .. tostring(result.code),
          "",
        }
        vim.list_extend(lines, vim.split(normalized, "\n", { plain = true }))
        open_scratch_window("checkhealth", lines, "checkhealth")
        vim.notify(
          "Health check finished: " .. command,
          result.code == 0 and vim.log.levels.INFO or vim.log.levels.WARN,
          {
            title = "Health",
          }
        )
      end)
    end)
  end, {
    nargs = "*",
    desc = "Run checkhealth asynchronously and show the output in a normal scrollable buffer",
  })
end

return M
