local M = {}

local util = require("core.util")

local START_PLUGINS = {
  "nvim-web-devicons",
  "catppuccin",
  "which-key.nvim",
  "tokyonight.nvim",
  "kanagawa.nvim",
  "snacks.nvim",
  "flash.nvim",
  "nvim-spider",
  "mini.nvim",
  "noice.nvim",
  "bufferline.nvim",
  "gitsigns.nvim",
  "blink.lib",
  "blink.cmp",
  "friendly-snippets",
  "nvim-treesitter",
  "nvim-treesitter-context",
  "conform.nvim",
  "mason.nvim",
  "mason-lspconfig.nvim",
  "mason-tool-installer.nvim",
  "nvim-lspconfig",
  "SchemaStore.nvim",
  "plenary.nvim",
  "nui.nvim",
  "trouble.nvim",
  "todo-comments.nvim",
}

---@return table[]
local function collect_specs()
  local specs = require("pack.spec").get_base_specs()
  local user = require("user")
  local disabled = util.to_lookup(user.pack.disable)

  for _, entry in ipairs(util.load_table_modules("user.plugins")) do
    local module_specs = entry.spec or entry
    if vim.islist(module_specs) then
      vim.list_extend(specs, module_specs)
    else
      table.insert(specs, module_specs)
    end
  end

  vim.list_extend(specs, user.pack.add)

  return vim.tbl_filter(function(spec)
    local name = type(spec) == "table" and spec.name or nil
    return not (name and disabled[name])
  end, specs)
end

local function should_load_immediately(spec)
  local name = type(spec) == "table" and spec.name or nil
  return name ~= nil and vim.tbl_contains(START_PLUGINS, name)
end

local STARTUP_PACK_NOTIFY_DELAY_MS = 1200
local STARTUP_PACK_HEALTH_BATCH_SIZE = 8
local PACK_PLUGIN_ROOT = vim.fs.joinpath(vim.fn.stdpath("data"), "site", "pack", "core", "opt")

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

local function plugin_dir(name)
  return vim.fs.joinpath(PACK_PLUGIN_ROOT, name)
end

local function count_non_git_entries(path)
  local count = 0
  for name in vim.fs.dir(path) do
    if name ~= ".git" then
      count = count + 1
      break
    end
  end
  return count
end

local function is_invalid_head(path)
  local head_path = vim.fs.joinpath(path, ".git", "HEAD")
  if vim.fn.filereadable(head_path) == 0 then return false end

  local ok, lines = pcall(vim.fn.readfile, head_path)
  if not ok or not lines or not lines[1] then return false end

  return lines[1]:find(".invalid", 1, true) ~= nil
end

local function is_broken_plugin_dir(path)
  if vim.fn.isdirectory(path) == 0 then return false end
  if count_non_git_entries(path) == 0 then return true end
  return is_invalid_head(path)
end

local function cleanup_broken_plugin_dirs(specs)
  local cleaned = {}

  for _, spec in ipairs(specs) do
    local name = type(spec) == "table" and spec.name or nil
    if name then
      local path = plugin_dir(name)
      if is_broken_plugin_dir(path) then
        local delete_code = vim.fn.delete(path, "rf")
        if delete_code == 0 then
          cleaned[#cleaned + 1] = name
        end
      end
    end
  end

  if #cleaned > 0 then
    vim.schedule(function()
      vim.notify(
        "Removed broken plugin directories before bootstrap: " .. table.concat(cleaned, ", "),
        vim.log.levels.WARN,
        { title = "Pack Repair" }
      )
    end)
  end
end

local function collect_startup_pack_issues(specs, on_done)
  local issues = {}
  local index = 1

  local function finish()
    table.sort(issues, function(a, b)
      return a.name < b.name
    end)
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
          vim.schedule(function()
            vim.notify("Pack health check failed: " .. tostring(err), vim.log.levels.WARN, {
              title = "Pack Health",
            })
          end)
        end
      end, STARTUP_PACK_NOTIFY_DELAY_MS)
    end,
  })
end

function M.setup()
  require("pack.commands").setup()
  require("pack.ui").setup()

  if vim.env.NVIM_NO_PLUGINS == "1" then return end
  if vim.fn.executable("git") == 0 then
    vim.notify("`git` is not available on PATH, skipping vim.pack bootstrap.", vim.log.levels.WARN)
    return
  end

  local specs = collect_specs()
  cleanup_broken_plugin_dirs(specs)

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
  if not ok then
    vim.notify("vim.pack bootstrap failed: " .. err, vim.log.levels.ERROR)
  end

  schedule_startup_pack_health(load_errors, specs)
end

M.collect_specs = collect_specs
M.cleanup_broken_plugin_dirs = cleanup_broken_plugin_dirs

return M
