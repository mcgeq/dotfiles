local M = {}

function M.setup()
  if vim.env.NVIM_NO_PLUGINS == "1" then return end
  local catalog = require "pack.spec"
  local features = require("user").private.features
  require("core.tools").setup()
  for _, entry in ipairs(catalog.configurations()) do
    if
      (not entry.feature or features[entry.feature] ~= false)
      and (not entry.plugin or catalog.enabled(entry.plugin))
    then
      require("plugins." .. entry.module).setup()
    end
  end
  for _, entry in ipairs(require("core.util").load_table_modules "user.plugins") do
    if type(entry.setup) == "function" and (not entry.feature or features[entry.feature] ~= false) then
      local enabled = true
      for _, spec in ipairs(catalog.extension_specs(entry)) do
        if not catalog.enabled(spec.name, spec) then enabled = false end
      end
      if enabled then entry.setup() end
    end
  end
end

return M
