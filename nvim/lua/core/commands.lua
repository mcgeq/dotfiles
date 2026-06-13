local M = {}
local scratch = require("core.scratch")

local function open_scratch_window(name, lines, filetype)
  return scratch.open(name, lines, { filetype = filetype })
end

local function format_client_lines(bufnr)
  local clients = vim.lsp.get_clients({ bufnr = bufnr })
  local lines = {
    "LSP clients for current buffer",
    "",
    "Buffer:   " .. vim.api.nvim_buf_get_name(bufnr),
    "Filetype: " .. vim.bo[bufnr].filetype,
    "Root:     " .. (vim.fs.root(bufnr, { ".clangd", "compile_commands.json", "compile_flags.txt", "CMakeLists.txt", ".git" }) or "n/a"),
    "Log:      " .. vim.lsp.log.get_filename(),
    "",
  }

  if vim.tbl_isempty(clients) then
    table.insert(lines, "No clients attached to this buffer.")
    return lines
  end

  table.insert(lines, "Attached clients:")
  for _, client in ipairs(clients) do
    table.insert(lines, "")
    table.insert(lines, "- " .. client.name .. " (id " .. client.id .. ")")
    table.insert(lines, "  Root: " .. (client.root_dir or "n/a"))
    table.insert(lines, "  Cmd:  " .. table.concat(client.config.cmd or {}, " "))
    table.insert(lines, "  Filetypes: " .. table.concat(client.config.filetypes or {}, ", "))
  end

  return lines
end

local function unique_names(items)
  local seen = {}
  local names = {}
  for _, item in ipairs(items) do
    local name = type(item) == "table" and item.name or item
    if name and not seen[name] then
      seen[name] = true
      table.insert(names, name)
    end
  end
  table.sort(names)
  return names
end

local function available_config_names(arg_lead)
  local names = unique_names(vim.lsp.get_configs())
  if not arg_lead or arg_lead == "" then return names end
  return vim.tbl_filter(function(name)
    return name:find(arg_lead, 1, true) == 1
  end, names)
end

local function buffer_clients(bufnr, name)
  return vim.lsp.get_clients(vim.tbl_extend("force", { bufnr = bufnr }, name and { name = name } or {}))
end

local function names_for_filetype(filetype)
  return unique_names(vim.lsp.get_configs({ filetype = filetype, enabled = true }))
end

local function stop_buffer_clients(bufnr, name)
  local clients = buffer_clients(bufnr, name)
  if vim.tbl_isempty(clients) then return {}, {} end

  local ids = {}
  local names = {}
  for _, client in ipairs(clients) do
    table.insert(ids, client.id)
    table.insert(names, client.name)
  end
  vim.lsp.stop_client(ids, true)
  return ids, unique_names(names)
end

local function current_init_path()
  local init_path = vim.env.MYVIMRC
  if type(init_path) == "string" and init_path ~= "" and vim.fn.filereadable(init_path) == 1 then return init_path end
  return vim.fs.joinpath(vim.fn.stdpath("config"), "init.lua")
end

local function combine_process_output(result)
  local parts = {}
  if result.stdout and result.stdout ~= "" then table.insert(parts, result.stdout) end
  if result.stderr and result.stderr ~= "" then table.insert(parts, result.stderr) end
  return table.concat(parts, "\n")
end

function M.setup()
  local frontend_state = require("lang.frontend_state")
  local sessions = require("core.sessions")
  local terminal = require("core.terminal")
  local workspace = require("core.workspace")
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

  vim.api.nvim_create_user_command("TerminalCwd", function()
    terminal.open_cwd()
  end, { desc = "Open a floating shell in the current working directory" })

  vim.api.nvim_create_user_command("TerminalProject", function()
    terminal.open_project()
  end, { desc = "Open a floating shell in the current project root" })

  vim.api.nvim_create_user_command("TerminalBufferDir", function()
    terminal.open_buffer_dir()
  end, { desc = "Open a floating shell in the current buffer directory" })

  vim.api.nvim_create_user_command("TerminalLast", function()
    terminal.open_last()
  end, { desc = "Reopen the last floating terminal command" })

  vim.api.nvim_create_user_command("SessionInfo", function()
    sessions.show_current()
  end, { desc = "Show the current workspace session target" })

  vim.api.nvim_create_user_command("SessionSave", function()
    sessions.save_current()
  end, { desc = "Save the current workspace session" })

  vim.api.nvim_create_user_command("SessionLoad", function()
    sessions.load_current()
  end, { desc = "Load the current workspace session" })

  vim.api.nvim_create_user_command("SessionSelect", function()
    sessions.select("read")
  end, { desc = "Pick and load a saved workspace session" })

  vim.api.nvim_create_user_command("SessionDelete", function()
    sessions.delete_current()
  end, { desc = "Delete the current workspace session" })

  vim.api.nvim_create_user_command("SessionSelectDelete", function()
    sessions.select("delete")
  end, { desc = "Pick and delete a saved workspace session" })

  vim.api.nvim_create_user_command("SessionRestart", function()
    sessions.restart()
  end, { desc = "Restart Neovim and restore the current workspace session" })

  vim.api.nvim_create_user_command("ProjectInfo", function()
    workspace.show_info()
  end, { desc = "Show current project and root detection details" })

  vim.api.nvim_create_user_command("ProjectRoot", function()
    workspace.cd_to_current_root()
  end, { desc = "Set the current working directory to the current buffer project root" })

  vim.api.nvim_create_user_command("ProjectSelect", function()
    workspace.select()
  end, { desc = "Pick a known project and switch the current working directory" })

  vim.api.nvim_create_user_command("ProjectSessionSelect", function()
    workspace.select({ load_session = true })
  end, { desc = "Pick a known project, switch to it, and load its saved session if present" })

  vim.api.nvim_create_user_command("Health", function(opts)
    local command = opts.args ~= "" and ("checkhealth " .. opts.args) or "checkhealth"
    local init_path = current_init_path()

    local argv = { vim.v.progpath, "--headless", "-i", "NONE" }
    if vim.fn.filereadable(init_path) == 1 then
      vim.list_extend(argv, { "-u", init_path })
    end
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
        vim.notify("Health check finished: " .. command, result.code == 0 and vim.log.levels.INFO or vim.log.levels.WARN, {
          title = "Health",
        })
      end)
    end)
  end, {
    nargs = "*",
    desc = "Run checkhealth asynchronously and show the output in a normal scrollable buffer",
  })

  vim.api.nvim_create_user_command("LspInfo", function()
    open_scratch_window("lspinfo", format_client_lines(vim.api.nvim_get_current_buf()), "lspinfo")
  end, { desc = "Show LSP status for the current buffer" })

  vim.api.nvim_create_user_command("LspLog", function()
    local log_path = vim.lsp.log.get_filename()
    if vim.fn.filereadable(log_path) == 0 then
      vim.notify("LSP log file does not exist yet: " .. log_path, vim.log.levels.WARN, { title = "LspLog" })
      return
    end
    local raw_lines = vim.fn.readfile(log_path)
    local total = #raw_lines
    local max_lines = 400
    local start = math.max(1, total - max_lines + 1)
    local lines = {
      "LSP log (latest first)",
      "",
      "Path: " .. log_path,
      string.format("Showing %d of %d lines", total - start + 1, total),
      "",
    }
    for i = total, start, -1 do
      table.insert(lines, raw_lines[i])
    end
    open_scratch_window("lsplog", lines, "log")
  end, { desc = "Open a latest-first LSP log view" })

  vim.api.nvim_create_user_command("LspLogRaw", function()
    local log_path = vim.lsp.log.get_filename()
    if vim.fn.filereadable(log_path) == 0 then
      vim.notify("LSP log file does not exist yet: " .. log_path, vim.log.levels.WARN, { title = "LspLogRaw" })
      return
    end
    vim.cmd.edit(vim.fn.fnameescape(log_path))
  end, { desc = "Open the raw LSP client log file" })

  vim.api.nvim_create_user_command("LspStart", function(opts)
    local targets = opts.args ~= "" and { opts.args } or names_for_filetype(vim.bo[0].filetype)
    if vim.tbl_isempty(targets) then
      vim.notify("No enabled LSP configs match filetype: " .. vim.bo[0].filetype, vim.log.levels.WARN, { title = "LspStart" })
      return
    end
    vim.lsp.enable(targets)
    vim.notify("LSP start -> " .. table.concat(targets, ", "), vim.log.levels.INFO, { title = "LspStart" })
  end, {
    nargs = "?",
    complete = function(arg_lead)
      return available_config_names(arg_lead)
    end,
    desc = "Start LSP for current filetype or the named config",
  })

  vim.api.nvim_create_user_command("LspStop", function(opts)
    local _, names = stop_buffer_clients(vim.api.nvim_get_current_buf(), opts.args ~= "" and opts.args or nil)
    if vim.tbl_isempty(names) then
      vim.notify("No matching LSP clients attached to this buffer.", vim.log.levels.WARN, { title = "LspStop" })
      return
    end
    vim.notify("LSP stop -> " .. table.concat(names, ", "), vim.log.levels.INFO, { title = "LspStop" })
  end, {
    nargs = "?",
    complete = function(arg_lead)
      return available_config_names(arg_lead)
    end,
    desc = "Stop attached LSP clients for the current buffer",
  })

  vim.api.nvim_create_user_command("LspRestart", function(opts)
    local bufnr = vim.api.nvim_get_current_buf()
    local target_name = opts.args ~= "" and opts.args or nil
    local _, names = stop_buffer_clients(bufnr, target_name)
    if vim.tbl_isempty(names) then
      names = target_name and { target_name } or names_for_filetype(vim.bo[bufnr].filetype)
    end
    if vim.tbl_isempty(names) then
      vim.notify("No matching LSP configs found for restart.", vim.log.levels.WARN, { title = "LspRestart" })
      return
    end
    vim.defer_fn(function()
      vim.lsp.enable(names)
      vim.notify("LSP restart -> " .. table.concat(names, ", "), vim.log.levels.INFO, { title = "LspRestart" })
    end, 100)
  end, {
    nargs = "?",
    complete = function(arg_lead)
      return available_config_names(arg_lead)
    end,
    desc = "Restart attached LSP clients for the current buffer",
  })

  local user_commands = require("core.util").require_if_exists("user.commands")
  if type(user_commands) == "function" then user_commands() end
end

return M
