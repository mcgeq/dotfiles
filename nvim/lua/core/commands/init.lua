local M = {}

local scratch = require "core.scratch"

local function open_scratch_window(name, lines, filetype) return scratch.open(name, lines, { filetype = filetype }) end

function M.setup()
  local frontend_state = require "lang.frontend_state"
  local sessions = require "core.sessions"
  local terminal = require "core.terminal"
  local workspace = require "core.workspace"
  local ui = require "plugins.ui"

  vim.api.nvim_create_user_command("NvimConfig", function()
    local snacks = require("core.util").require_if_exists "snacks"
    if snacks and snacks.picker then
      snacks.picker.files { cwd = vim.fn.stdpath "config" }
    else
      vim.cmd.edit(vim.fn.stdpath "config" .. "/init.lua")
    end
  end, { desc = "Open Neovim config" })

  vim.api.nvim_create_user_command("NvimUserConfig", function()
    local user_dir = vim.fn.stdpath "config" .. "/lua/user"
    local snacks = require("core.util").require_if_exists "snacks"
    if snacks and snacks.picker then
      snacks.picker.files { cwd = user_dir }
    else
      vim.cmd.edit(user_dir .. "/init.lua")
    end
  end, { desc = "Open user extension config" })

  vim.api.nvim_create_user_command("NvimStatus", function()
    local lines = {
      "Neovim native config is active",
      "",
      "Config: " .. vim.fn.stdpath "config",
      "Data:   " .. vim.fn.stdpath "data",
      "User:   " .. vim.fn.stdpath "config" .. "/lua/user",
      "Pack:   " .. vim.fn.stdpath "config" .. "/nvim-pack-lock.json",
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

    vim.notify(
      "Frontend mode -> " .. frontend_state.describe(opts.args),
      vim.log.levels.INFO,
      { title = "Frontend Mode" }
    )
  end, {
    nargs = "?",
    complete = function() return frontend_state.available_modes() end,
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
    if ok then vim.notify("Theme -> " .. name, vim.log.levels.INFO, { title = "Theme" }) end
  end, {
    nargs = "?",
    complete = function(arg_lead) return ui.available_themes(arg_lead) end,
    desc = "Get or set the active theme",
  })

  vim.api.nvim_create_user_command("ThemeCycle", function()
    local name = ui.cycle_theme()
    vim.notify("Theme -> " .. name, vim.log.levels.INFO, { title = "Theme" })
  end, { desc = "Cycle bundled themes" })

  vim.api.nvim_create_user_command(
    "MarkdownTableFormat",
    function() require("lang.markdown").manual_format() end,
    { desc = "Format current markdown table" }
  )

  vim.api.nvim_create_user_command(
    "TerminalCwd",
    function() terminal.open_cwd() end,
    { desc = "Open a floating shell in the current working directory" }
  )

  vim.api.nvim_create_user_command(
    "TerminalProject",
    function() terminal.open_project() end,
    { desc = "Open a floating shell in the current project root" }
  )

  vim.api.nvim_create_user_command(
    "TerminalBufferDir",
    function() terminal.open_buffer_dir() end,
    { desc = "Open a floating shell in the current buffer directory" }
  )

  vim.api.nvim_create_user_command(
    "TerminalLast",
    function() terminal.open_last() end,
    { desc = "Reopen the last floating terminal command" }
  )

  vim.api.nvim_create_user_command(
    "SessionInfo",
    function() sessions.show_current() end,
    { desc = "Show the current workspace session target" }
  )

  vim.api.nvim_create_user_command(
    "SessionSave",
    function() sessions.save_current() end,
    { desc = "Save the current workspace session" }
  )

  vim.api.nvim_create_user_command(
    "SessionLoad",
    function() sessions.load_current() end,
    { desc = "Load the current workspace session" }
  )

  vim.api.nvim_create_user_command(
    "SessionSelect",
    function() sessions.select "read" end,
    { desc = "Pick and load a saved workspace session" }
  )

  vim.api.nvim_create_user_command(
    "SessionDelete",
    function() sessions.delete_current() end,
    { desc = "Delete the current workspace session" }
  )

  vim.api.nvim_create_user_command(
    "SessionSelectDelete",
    function() sessions.select "delete" end,
    { desc = "Pick and delete a saved workspace session" }
  )

  vim.api.nvim_create_user_command(
    "SessionRestart",
    function() sessions.restart() end,
    { desc = "Restart Neovim and restore the current workspace session" }
  )

  vim.api.nvim_create_user_command(
    "ProjectInfo",
    function() workspace.show_info() end,
    { desc = "Show current project and root detection details" }
  )

  vim.api.nvim_create_user_command(
    "ProjectRoot",
    function() workspace.cd_to_current_root() end,
    { desc = "Set the current working directory to the current buffer project root" }
  )

  vim.api.nvim_create_user_command(
    "ProjectSelect",
    function() workspace.select() end,
    { desc = "Pick a known project and switch the current working directory" }
  )

  vim.api.nvim_create_user_command(
    "ProjectSessionSelect",
    function() workspace.select { load_session = true } end,
    { desc = "Pick a known project, switch to it, and load its saved session if present" }
  )

  require("core.commands.lsp").setup(open_scratch_window)
  require("core.commands.health").setup(open_scratch_window)

  local user_commands = require("core.util").require_if_exists "user.commands"
  if type(user_commands) == "function" then user_commands() end
end

return M
