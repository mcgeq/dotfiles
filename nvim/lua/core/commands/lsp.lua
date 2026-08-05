local M = {}

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
  return vim.tbl_filter(function(name) return name:find(arg_lead, 1, true) == 1 end, names)
end

local function buffer_clients(bufnr, name)
  return vim.lsp.get_clients(vim.tbl_extend("force", { bufnr = bufnr }, name and { name = name } or {}))
end

local function names_for_filetype(filetype)
  return unique_names(vim.lsp.get_configs { filetype = filetype, enabled = true })
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

local function format_client_lines(bufnr)
  local clients = vim.lsp.get_clients { bufnr = bufnr }
  local lines = {
    "LSP clients for current buffer",
    "",
    "Buffer:   " .. vim.api.nvim_buf_get_name(bufnr),
    "Filetype: " .. vim.bo[bufnr].filetype,
    "Root:     " .. (vim.fs.root(
      bufnr,
      { ".clangd", "compile_commands.json", "compile_flags.txt", "CMakeLists.txt", ".git" }
    ) or "n/a"),
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

function M.setup(open_scratch_window)
  vim.api.nvim_create_user_command(
    "LspInfo",
    function() open_scratch_window("lspinfo", format_client_lines(vim.api.nvim_get_current_buf()), "lspinfo") end,
    { desc = "Show LSP status for the current buffer" }
  )

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
      vim.notify(
        "No enabled LSP configs match filetype: " .. vim.bo[0].filetype,
        vim.log.levels.WARN,
        { title = "LspStart" }
      )
      return
    end
    vim.lsp.enable(targets)
    vim.notify("LSP start -> " .. table.concat(targets, ", "), vim.log.levels.INFO, { title = "LspStart" })
  end, {
    nargs = "?",
    complete = function(arg_lead) return available_config_names(arg_lead) end,
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
    complete = function(arg_lead) return available_config_names(arg_lead) end,
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
    complete = function(arg_lead) return available_config_names(arg_lead) end,
    desc = "Restart attached LSP clients for the current buffer",
  })
end

return M
