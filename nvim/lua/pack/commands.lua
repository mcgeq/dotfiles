local M = {}
local PACK_PLUGIN_ROOT = vim.fs.joinpath(vim.fn.stdpath("data"), "site", "pack", "core", "opt")

local function path_exists(path)
  return path and vim.uv.fs_stat(path) ~= nil
end

local function plugin_dir(name)
  return vim.fs.joinpath(PACK_PLUGIN_ROOT, name)
end

local function worktree_state(path)
  if not path_exists(path) then return "missing" end

  local entries = 0
  for name in vim.fs.dir(path) do
    if name ~= ".git" then
      entries = entries + 1
      break
    end
  end

  if entries == 0 then return "broken" end
  return "ok"
end

local function shorten_rev(rev)
  if not rev or rev == "" then return "unknown" end
  return rev:sub(1, 8)
end

local function shorten_src(src)
  if not src or src == "" then return "" end
  local repo = src:match("github%.com[:/](.-)$") or src
  repo = repo:gsub("%.git$", "")
  return repo
end

local function plugin_status(plugin)
  local state = worktree_state(plugin.path)
  if state == "broken" then return "broken" end
  if state == "missing" then return "missing" end
  return plugin.active and "active" or "installed"
end

local function status_icon(status)
  if status == "active" then return "[A]" end
  if status == "installed" then return "[I]" end
  if status == "broken" then return "[!]" end
  if status == "missing" then return "[?]" end
  return "[ ]"
end

local function get_pack_init()
  local ok_pack, pack = pcall(require, "pack.init")
  if not ok_pack then return nil end
  if type(pack.collect_specs) ~= "function" then return nil end
  return pack
end

local function collect_specs(names)
  local pack = get_pack_init()
  if not pack then return {} end

  local specs = pack.collect_specs()
  if type(names) ~= "table" then return specs end

  local wanted = {}
  for _, name in ipairs(names) do
    if type(name) == "string" and name ~= "" then wanted[name] = true end
  end

  return vim.tbl_filter(function(spec)
    local name = type(spec) == "table" and spec.name or nil
    return name ~= nil and wanted[name] == true
  end, specs)
end

local function spec_name_list(names)
  local plugin_names = {}
  for _, spec in ipairs(collect_specs(names)) do
    local name = type(spec) == "table" and spec.name or nil
    if name then plugin_names[#plugin_names + 1] = name end
  end

  table.sort(plugin_names)
  return plugin_names
end

local function make_plugin_record(name, spec, plugin)
  local plugin_spec = plugin and plugin.spec or spec or {}
  return {
    name = name,
    active = plugin and plugin.active or false,
    path = plugin and plugin.path or plugin_dir(name),
    rev = plugin and plugin.rev or nil,
    src = plugin_spec.src,
  }
end

local function get_plugins(names)
  local plugins = {}
  local seen = {}
  local specs = collect_specs(names)
  local specs_by_name = {}
  local lookup_names = {}

  for _, spec in ipairs(specs) do
    local name = type(spec) == "table" and spec.name or nil
    if name and not seen[name] then
      seen[name] = true
      specs_by_name[name] = spec
      lookup_names[#lookup_names + 1] = name
    end
  end

  for _, name in ipairs(names or {}) do
    if type(name) == "string" and name ~= "" and not seen[name] then
      seen[name] = true
      lookup_names[#lookup_names + 1] = name
    end
  end

  if names == nil and #lookup_names == 0 then
    local ok_get, all = pcall(vim.pack.get, nil, { info = true })
    if ok_get then
      for _, plugin in ipairs(all) do
        local name = plugin.spec.name
        if name then
          plugins[#plugins + 1] = make_plugin_record(name, plugin.spec, plugin)
        end
      end
    end
  else
    for _, name in ipairs(lookup_names) do
      local ok_get, result = pcall(vim.pack.get, { name }, { info = true })
      local plugin = ok_get and result and result[1] or nil
      plugins[#plugins + 1] = make_plugin_record(name, specs_by_name[name], plugin)
    end
  end

  table.sort(plugins, function(a, b)
    return a.name < b.name
  end)

  return plugins
end

local function cleanup_plugin_dirs(names)
  if vim.env.NVIM_NO_PLUGINS == "1" then return end

  local pack = get_pack_init()
  if not pack or type(pack.cleanup_broken_plugin_dirs) ~= "function" then return end

  local specs = collect_specs(names)
  pack.cleanup_broken_plugin_dirs(specs)
end

local function get_plugin_names(arg_lead)
  local names = {}
  for _, name in ipairs(spec_name_list()) do
    if name and name:lower():find(arg_lead:lower(), 1, true) == 1 then
      table.insert(names, name)
    end
  end
  table.sort(names)
  return names
end

local function pick_plugin_and_run(title, plugins, on_choice, opts)
  opts = opts or {}
  local items = {
    {
      name = "__all__",
      text = "All plugins",
      display = opts.all_display or "[*] all installed plugins",
      src = opts.all_src or "Update every installed vim.pack plugin",
    },
  }

  for _, plugin in ipairs(plugins) do
    local status = plugin_status(plugin)
    local rev = shorten_rev(plugin.rev)
    local repo = shorten_src(plugin.src)
    items[#items + 1] = {
      name = plugin.name,
      text = plugin.name,
      display = string.format("%s %-22s %s  %s", status_icon(status), plugin.name, rev, repo),
      status = status,
      rev = rev,
      src = plugin.src,
      path = plugin.path,
    }
  end

  if #plugins == 0 then
    vim.notify(opts.empty_message or "No plugins available", vim.log.levels.INFO, { title = title })
    return
  end

  vim.ui.select(items, {
    prompt = string.format("%s (%d)", title, #plugins),
    format_item = function(item)
      return item.display or item.text
    end,
    kind = "pack",
    snacks = {
      layout = { preset = "select" },
      matcher = { fuzzy = true, case_mode = "smart_case" },
      preview = "none",
    },
  }, function(choice)
    if not choice then return end
    if choice.name == "__all__" then
      on_choice(nil)
      return
    end
    on_choice({ choice.name })
  end)
end

local function filter_plugins(predicate)
  local ret = {}
  for _, plugin in ipairs(get_plugins()) do
    if predicate(plugin) then
      ret[#ret + 1] = plugin
    end
  end
  return ret
end

local function get_repair_candidates()
  return filter_plugins(function(plugin)
    local status = plugin_status(plugin)
    return status == "broken" or status == "missing"
  end)
end

local function pending_repair_path()
  return vim.fs.joinpath(vim.fn.stdpath("data"), "pack-repair-pending.json")
end

local function summarize_names(names, max_items)
  local lines = {}
  local limit = max_items or 6

  for index, name in ipairs(names or {}) do
    if index > limit then
      lines[#lines + 1] = ("... and %d more"):format(#names - limit)
      break
    end

    lines[#lines + 1] = name
  end

  return lines
end

local function save_pending_repair(names)
  local ok_encode, payload = pcall(vim.json.encode, names)
  if not ok_encode then return false, payload end

  local path = pending_repair_path()
  local ok_write, err = pcall(vim.fn.writefile, { payload }, path)
  if not ok_write then return false, err end

  return true, path
end

local function load_pending_repair()
  local path = pending_repair_path()
  if vim.fn.filereadable(path) == 0 then return nil, path end

  local ok_read, lines = pcall(vim.fn.readfile, path)
  if not ok_read then return nil, lines end

  local payload = table.concat(lines, "\n")
  if payload == "" then return {}, path end

  local ok_decode, names = pcall(vim.json.decode, payload)
  if not ok_decode or type(names) ~= "table" then return nil, names end

  local ret = {}
  for _, name in ipairs(names) do
    if type(name) == "string" and name ~= "" then ret[#ret + 1] = name end
  end

  return ret, path
end

local function clear_pending_repair()
  local path = pending_repair_path()
  if vim.fn.filereadable(path) == 0 then return true, path end

  local ok_delete, code = pcall(vim.fn.delete, path)
  if not ok_delete then return false, code end
  if code ~= 0 then return false, code end

  return true, path
end

local function open_pending_repair_session()
  local names = load_pending_repair()
  if type(names) ~= "table" or #names == 0 then
    vim.notify("No pending plugin repair list saved.", vim.log.levels.INFO, { title = "Pack Repair" })
    return false
  end

  local terminal = require("core.terminal")
  local env = vim.fn.environ()
  env.NVIM_NO_PLUGINS = "1"

  terminal.open_float({ vim.v.progpath, "+PackRepairPending" }, {
    cwd = vim.uv.cwd(),
    env = env,
    title = " pack repair (clean session) ",
    width_ratio = 0.92,
    height_ratio = 0.9,
  })

  return true
end

local function notify_repair_candidates(opts)
  opts = opts or {}

  local candidates = get_repair_candidates()
  if #candidates == 0 then return false end

  local max_items = opts.max_items or 6
  local lines = {
    "Some plugins are missing or broken:",
  }

  for index, plugin in ipairs(candidates) do
    if index > max_items then
      lines[#lines + 1] = ("... and %d more"):format(#candidates - max_items)
      break
    end

    local status = plugin_status(plugin)
    lines[#lines + 1] = ("%s %s"):format(status_icon(status), plugin.name)
  end

  lines[#lines + 1] = ""
  lines[#lines + 1] = "Run :PackRepair to reinstall them."

  vim.notify(table.concat(lines, "\n"), opts.level or vim.log.levels.WARN, {
    title = opts.title or "Pack Health",
  })

  return true
end

local function repair_plugins(names, opts)
  opts = opts or {}
  local used_pending = opts.pending == true

  if names == nil then
    if vim.env.NVIM_NO_PLUGINS == "1" then
      local pending_names = load_pending_repair()
      if type(pending_names) == "table" and #pending_names > 0 then
        names = pending_names
        used_pending = true
      end
    end

    cleanup_plugin_dirs(names)

    if names == nil then
      names = vim.tbl_map(function(plugin)
        return plugin.name
      end, get_repair_candidates())
    end
  else
    cleanup_plugin_dirs(names)
  end

  local plugins = get_plugins(names)
  local by_name = {}
  for _, plugin in ipairs(plugins) do
    by_name[plugin.name] = plugin
  end

  local targets = {}
  for _, name in ipairs(names or {}) do
    local plugin = by_name[name]
    if plugin then
      targets[#targets + 1] = plugin
    end
  end

  if #targets == 0 then
    if used_pending then
      clear_pending_repair()
      vim.notify("No pending plugins still need repair.", vim.log.levels.INFO, { title = "Pack Repair" })
      return
    end

    vim.notify("No repairable plugins selected", vim.log.levels.WARN, { title = "Pack Repair" })
    return
  end

  local active_targets = {}
  local inactive_targets = {}
  for _, plugin in ipairs(targets) do
    if plugin.active then
      active_targets[#active_targets + 1] = plugin
    else
      inactive_targets[#inactive_targets + 1] = plugin
    end
  end

  if #active_targets > 0 then
    local active_names = vim.tbl_map(function(plugin)
      return plugin.name
    end, active_targets)
    local ok_save, save_result = save_pending_repair(active_names)
    local lines = {
      "Active plugins cannot be reinstalled in the current session:",
    }
    vim.list_extend(lines, summarize_names(active_names))
    lines[#lines + 1] = ""

    if ok_save then
      lines[#lines + 1] = ("Saved %d plugin(s) for the next repair session."):format(#active_names)
      lines[#lines + 1] = "Run `:PackRepairClean` to finish in a clean nested session."
      lines[#lines + 1] = "Manual fallback: start Neovim with `NVIM_NO_PLUGINS=1` and run `:PackRepairPending`."
    else
      lines[#lines + 1] = "Could not save the pending repair list:"
      lines[#lines + 1] = tostring(save_result)
    end

    vim.notify(table.concat(lines, "\n"), vim.log.levels.WARN, { title = "Pack Repair" })
  end

  if #inactive_targets == 0 then return end

  local target_names = {}
  local target_specs = {}
  for _, plugin in ipairs(inactive_targets) do
    target_names[#target_names + 1] = plugin.name
    target_specs[#target_specs + 1] = {
      name = plugin.name,
      src = plugin.src,
    }
  end

  local ok_del, err_del = pcall(vim.pack.del, target_names)
  if not ok_del then
    vim.notify("Failed to delete broken plugins: " .. tostring(err_del), vim.log.levels.ERROR, { title = "Pack Repair" })
    return
  end

  local ok_add, err_add = pcall(vim.pack.add, target_specs, { load = true, confirm = false })
  if not ok_add then
    vim.notify("Failed to reinstall plugins: " .. tostring(err_add), vim.log.levels.ERROR, { title = "Pack Repair" })
    return
  end

  if used_pending then clear_pending_repair() end

  vim.notify("Repaired: " .. table.concat(target_names, ", "), vim.log.levels.INFO, { title = "Pack Repair" })
end

function M.setup()
  local map = require("core.keymaps").map

  vim.api.nvim_create_user_command("PackUpdate", function(opts)
    if #opts.fargs > 0 then
      vim.pack.update(opts.fargs)
      return
    end

    if opts.bang then
      vim.pack.update()
      return
    end

    pick_plugin_and_run("Pack Update", get_plugins(), function(names)
      vim.pack.update(names)
    end)
  end, {
    bang = true,
    nargs = "*",
    complete = get_plugin_names,
    desc = "Update vim.pack plugins (! updates all)",
  })

  vim.api.nvim_create_user_command("PackStatus", function(opts)
    if #opts.fargs > 0 then
      vim.pack.update(opts.fargs, { offline = true })
      return
    end

    if opts.bang then
      vim.pack.update(nil, { offline = true })
      return
    end

    pick_plugin_and_run("Pack Status", get_plugins(), function(names)
      vim.pack.update(names, { offline = true })
    end)
  end, {
    bang = true,
    nargs = "*",
    complete = get_plugin_names,
    desc = "Show vim.pack plugin status (! checks all)",
  })

  vim.api.nvim_create_user_command("PackRepair", function(opts)
    if #opts.fargs > 0 then
      repair_plugins(opts.fargs)
      return
    end

    local candidates = get_repair_candidates()

    if opts.bang then
      repair_plugins(vim.tbl_map(function(plugin)
        return plugin.name
      end, candidates))
      return
    end

    pick_plugin_and_run("Pack Repair", candidates, function(names)
      repair_plugins(names)
    end, {
      all_display = "[*] all broken or missing plugins",
      all_src = "Repair every broken or missing vim.pack plugin",
      empty_message = "No broken or missing plugins found",
    })
  end, {
    bang = true,
    nargs = "*",
    complete = get_plugin_names,
    desc = "Repair broken vim.pack plugins (! repairs all broken ones)",
  })

  vim.api.nvim_create_user_command("PackRepairPending", function()
    local names = load_pending_repair()
    if type(names) ~= "table" or #names == 0 then
      vim.notify("No pending plugin repair list saved.", vim.log.levels.INFO, { title = "Pack Repair" })
      return
    end

    repair_plugins(names, { pending = true })
  end, {
    desc = "Repair plugins saved from a previous active session",
  })

  vim.api.nvim_create_user_command("PackRepairClean", function()
    repair_plugins()

    if not open_pending_repair_session() then
      vim.notify("Current session repaired everything it could without a clean restart.", vim.log.levels.INFO, {
        title = "Pack Repair",
      })
    end
  end, {
    desc = "Repair plugins, then open a clean nested Neovim session for active ones",
  })

  vim.api.nvim_create_user_command("PackSync", function()
    vim.pack.update(nil, { target = "lockfile" })
  end, {
    desc = "Sync local plugins to nvim-pack-lock.json",
  })

  map("n", "<leader>pl", "<cmd>PackOpenLog<cr>", "Open pack log")
  map("n", "<leader>pr", "<cmd>PackRepair<cr>", "Pack repair")
  map("n", "<leader>ps", "<cmd>PackStatus<cr>", "Pack status")
  map("n", "<leader>pu", "<cmd>PackUpdate<cr>", "Pack update")
  map("n", "<leader>pP", "<cmd>PackUpdate!<cr>", "Pack update all")
  map("n", "<leader>pR", "<cmd>PackRepairPending<cr>", "Pack repair pending")
  map("n", "<leader>pC", "<cmd>PackRepairClean<cr>", "Pack repair clean")
  map("n", "<leader>pU", "<cmd>PackSync<cr>", "Pack sync lockfile")
end

M.get_plugins = get_plugins
M.get_repair_candidates = get_repair_candidates
M.notify_repair_candidates = notify_repair_candidates
M.load_pending_repair = load_pending_repair

return M
