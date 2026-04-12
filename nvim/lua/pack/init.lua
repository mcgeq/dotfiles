local M = {}

local util = require("core.util")

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

function M.setup()
  require("pack.commands").setup()
  require("pack.ui").setup()

  if vim.env.NVIM_NO_PLUGINS == "1" then return end
  if vim.fn.executable("git") == 0 then
    vim.notify("`git` is not available on PATH, skipping vim.pack bootstrap.", vim.log.levels.WARN)
    return
  end

  local ok, err = pcall(vim.pack.add, collect_specs(), { load = true, confirm = true })
  if not ok then
    vim.notify("vim.pack bootstrap failed: " .. err, vim.log.levels.ERROR)
  end
end

return M
