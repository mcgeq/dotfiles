local M = {}

local function collect_specs()
  local catalog = require "pack.spec"
  local specs = {}
  for _, entry in ipairs(catalog.entries()) do
    if catalog.enabled(entry.name, entry) then specs[#specs + 1] = catalog.native_spec(entry) end
  end
  return specs
end

local function should_load_immediately(spec)
  local catalog = require "pack.spec"
  for _, entry in ipairs(catalog.entries()) do
    if entry.name == spec.name then return entry.start == true and catalog.enabled(entry.name, entry) end
  end
  return false
end

local STARTUP_PACK_NOTIFY_DELAY_MS = 1200
local STARTUP_PACK_HEALTH_BATCH_SIZE = 8

local function dedupe_load_errors(errors)
  local seen = {}
  local ret = {}

  for _, item in ipairs(errors) do
    local key = item.name .. "\0" .. item.err
    if not seen[key] then
      seen[key] = true
      ret[#ret + 1] = item
    end
  end

  return ret
end

local function notify_load_errors(errors)
  local failures = dedupe_load_errors(errors)
  if #failures == 0 then return end

  local lines = {
    "Some start plugins failed to load:",
  }

  for index, item in ipairs(failures) do
    if index > 5 then
      lines[#lines + 1] = ("... and %d more"):format(#failures - 5)
      break
    end

    local summary = tostring(item.err):gsub("%s+", " ")
    if #summary > 140 then summary = summary:sub(1, 137) .. "..." end
    lines[#lines + 1] = ("- %s: %s"):format(item.name, summary)
  end

  lines[#lines + 1] = ""
  lines[#lines + 1] = "Try :PackRepair, :PackUpdate, or restart Neovim."

  vim.notify(table.concat(lines, "\n"), vim.log.levels.ERROR, {
    title = "Pack Load",
  })
end

local function plugin_dir(name) return require("pack.health").directory(name) end

local function is_broken_plugin_dir(path) return require("pack.health").status(path) == "broken" end

local function cleanup_broken_plugin_dirs(specs)
  local cleaned = {}

  for _, spec in ipairs(specs) do
    local name = type(spec) == "table" and spec.name or nil
    if name then
      local path = plugin_dir(name)
      if is_broken_plugin_dir(path) then
        local delete_code = vim.fn.delete(path, "rf")
        if delete_code == 0 then cleaned[#cleaned + 1] = name end
      end
    end
  end

  if #cleaned > 0 then
    vim.schedule(
      function()
        vim.notify(
          "Removed broken plugin directories before bootstrap: " .. table.concat(cleaned, ", "),
          vim.log.levels.WARN,
          { title = "Pack Repair" }
        )
      end
    )
  end
end

local function collect_startup_pack_issues(specs, on_done)
  local issues = {}
  local index = 1

  local function finish()
    table.sort(issues, function(a, b) return a.name < b.name end)
    on_done(issues)
  end

  local function step()
    local processed = 0

    while index <= #specs and processed < STARTUP_PACK_HEALTH_BATCH_SIZE do
      local spec = specs[index]
      index = index + 1
      processed = processed + 1

      local name = type(spec) == "table" and spec.name or nil
      if name then
        local path = plugin_dir(name)
        if vim.fn.isdirectory(path) == 0 then
          issues[#issues + 1] = { name = name, status = "missing" }
        elseif is_broken_plugin_dir(path) then
          issues[#issues + 1] = { name = name, status = "broken" }
        end
      end
    end

    if index <= #specs then
      vim.defer_fn(step, 1)
      return
    end

    finish()
  end

  step()
end

local function notify_startup_pack_issues(specs)
  collect_startup_pack_issues(specs, function(issues)
    if #issues == 0 then return end

    local lines = {
      "Some plugins are missing or broken:",
    }

    for issue_index, issue in ipairs(issues) do
      if issue_index > 6 then
        lines[#lines + 1] = ("... and %d more"):format(#issues - 6)
        break
      end

      local icon = issue.status == "missing" and "[?]" or "[!]"
      lines[#lines + 1] = ("%s %s"):format(icon, issue.name)
    end

    lines[#lines + 1] = ""
    lines[#lines + 1] = "Run :PackRepair to reinstall them."

    vim.notify(table.concat(lines, "\n"), vim.log.levels.WARN, {
      title = "Pack Health",
    })
  end)
end

local function schedule_startup_pack_health(load_errors, specs)
  vim.api.nvim_create_autocmd("VimEnter", {
    once = true,
    callback = function()
      vim.defer_fn(function()
        local ok, err = pcall(function()
          notify_load_errors(load_errors)
          notify_startup_pack_issues(specs)
        end)
        if not ok then
          vim.schedule(
            function()
              vim.notify("Pack health check failed: " .. tostring(err), vim.log.levels.WARN, {
                title = "Pack Health",
              })
            end
          )
        end
      end, STARTUP_PACK_NOTIFY_DELAY_MS)
    end,
  })
end

function M.setup()
  require("pack.commands").setup()
  require("pack.ui").setup()

  if vim.env.NVIM_NO_PLUGINS == "1" then return end
  if vim.fn.executable "git" == 0 then
    vim.notify("`git` is not available on PATH, skipping vim.pack bootstrap.", vim.log.levels.WARN)
    return
  end

  local specs = collect_specs()

  local load_errors = {}
  local ok, err = pcall(vim.pack.add, specs, {
    confirm = true,
    load = function(plugin)
      if not should_load_immediately(plugin.spec) then return end

      local ok_load, load_err = pcall(vim.cmd.packadd, plugin.spec.name)
      if not ok_load then
        load_errors[#load_errors + 1] = {
          name = plugin.spec.name,
          err = tostring(load_err),
        }
      end
    end,
  })
  if not ok then vim.notify("vim.pack bootstrap failed: " .. err, vim.log.levels.ERROR) end

  schedule_startup_pack_health(load_errors, specs)
end

M.collect_specs = collect_specs
M.cleanup_broken_plugin_dirs = cleanup_broken_plugin_dirs

return M
