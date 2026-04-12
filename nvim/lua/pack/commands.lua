local M = {}

local function path_exists(path)
  return path and (vim.uv or vim.loop).fs_stat(path) ~= nil
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

local function get_plugins()
  local plugins = {}

  for _, plugin in ipairs(vim.pack.get(nil, { info = true })) do
    local name = plugin.spec.name
    if name then
      plugins[#plugins + 1] = {
        name = name,
        active = plugin.active,
        path = plugin.path,
        rev = plugin.rev,
        src = plugin.spec.src,
      }
    end
  end

  table.sort(plugins, function(a, b)
    return a.name < b.name
  end)

  return plugins
end

local function get_plugin_names(arg_lead)
  local names = {}
  for _, plugin in ipairs(get_plugins()) do
    local name = plugin.name
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

local function repair_plugins(names)
  local plugins = get_plugins()
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
    local lines = {
      "Active plugins cannot be reinstalled in the current session:",
      table.concat(active_names, ", "),
      "",
      "Restart Neovim once with `NVIM_NO_PLUGINS=1` and run:",
      ":PackRepair " .. table.concat(active_names, " "),
    }
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
  map("n", "<leader>pU", "<cmd>PackSync<cr>", "Pack sync lockfile")
end

return M
