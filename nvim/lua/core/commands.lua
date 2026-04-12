local M = {}

function M.setup()
  local frontend_state = require("lang.frontend")
  local ui = require("plugins.ui")

  vim.api.nvim_create_user_command("NvimConfig", function()
    local snacks = require("core.util").require_if_exists("snacks")
    if snacks and snacks.picker then
      snacks.picker.files({ cwd = vim.fn.stdpath("config") })
    else
      vim.cmd.edit(vim.fn.stdpath("config") .. "/init.lua")
    end
  end, { desc = "Open Neovim config" })

  vim.api.nvim_create_user_command("NvimUserConfig", function()
    local user_dir = vim.fn.stdpath("config") .. "/lua/user"
    local snacks = require("core.util").require_if_exists("snacks")
    if snacks and snacks.picker then
      snacks.picker.files({ cwd = user_dir })
    else
      vim.cmd.edit(user_dir .. "/init.lua")
    end
  end, { desc = "Open user extension config" })

  vim.api.nvim_create_user_command("NvimStatus", function()
    local lines = {
      "Neovim native config is active",
      "",
      "Config: " .. vim.fn.stdpath("config"),
      "Data:   " .. vim.fn.stdpath("data"),
      "User:   " .. vim.fn.stdpath("config") .. "/lua/user",
      "Pack:   " .. vim.fn.stdpath("config") .. "/nvim-pack-lock.json",
      "Theme:  " .. ui.current_theme(),
      "Frontend mode: " .. frontend_state.describe(),
    }
    vim.notify(table.concat(lines, "\n"), vim.log.levels.INFO, { title = "Nvim Status" })
  end, { desc = "Show active config status" })

  vim.api.nvim_create_user_command("FrontendMode", function(opts)
    if opts.args == "" then
      vim.notify("Frontend mode: " .. frontend_state.describe(), vim.log.levels.INFO, { title = "Frontend Mode" })
      return
    end

    if not frontend_state.set_mode(opts.args) then
      vim.notify("Unknown frontend mode: " .. opts.args, vim.log.levels.ERROR, { title = "Frontend Mode" })
      return
    end

    vim.notify("Frontend mode -> " .. frontend_state.describe(opts.args), vim.log.levels.INFO, { title = "Frontend Mode" })
  end, {
    nargs = "?",
    complete = function()
      return frontend_state.available_modes()
    end,
    desc = "Get or set frontend save/format mode",
  })

  vim.api.nvim_create_user_command("FrontendModeCycle", function()
    local mode = frontend_state.cycle_mode()
    vim.notify("Frontend mode -> " .. frontend_state.describe(mode), vim.log.levels.INFO, { title = "Frontend Mode" })
  end, { desc = "Cycle frontend save/format mode" })

  vim.api.nvim_create_user_command("Theme", function(opts)
    if opts.args == "" then
      vim.notify("Theme: " .. ui.current_theme(), vim.log.levels.INFO, { title = "Theme" })
      return
    end

    local ok, name = ui.apply_theme(opts.args)
    if ok then
      vim.notify("Theme -> " .. name, vim.log.levels.INFO, { title = "Theme" })
    end
  end, {
    nargs = "?",
    complete = function(arg_lead)
      return ui.available_themes(arg_lead)
    end,
    desc = "Get or set the active theme",
  })

  vim.api.nvim_create_user_command("ThemeCycle", function()
    local name = ui.cycle_theme()
    vim.notify("Theme -> " .. name, vim.log.levels.INFO, { title = "Theme" })
  end, { desc = "Cycle bundled themes" })

  vim.api.nvim_create_user_command("MarkdownTableFormat", function()
    require("lang.markdown").manual_format()
  end, { desc = "Format current markdown table" })

  local user_commands = require("core.util").require_if_exists("user.commands")
  if type(user_commands) == "function" then user_commands() end
end

return M
