local M = {}

local function feature_enabled(name) return require("user").private.features[name] ~= false end

function M.setup()
  local util = require "core.util"

  require("plugins.ui").setup()
  require("plugins.statusline").setup()
  if feature_enabled "noice" then require("plugins.noice").setup() end
  require("plugins.snacks").setup()
  require("plugins.flash").setup()
  require("plugins.editing").setup()
  require("plugins.completion").setup()
  require("plugins.git").setup()
  require("plugins.syntax").setup()
  require("plugins.format").setup()
  require("plugins.backend").setup()
  if feature_enabled "frontend" then require("plugins.frontend").setup() end
  if feature_enabled "spectre" then require("plugins.search").setup() end
  if feature_enabled "web" then require("plugins.web").setup() end
  if feature_enabled "debug" then require("plugins.debug").setup() end
  require("plugins.workflow").setup()
  require("plugins.markdown").setup()

  for _, entry in ipairs(util.load_table_modules "user.plugins") do
    if type(entry.setup) == "function" then
      if entry.feature == nil or feature_enabled(entry.feature) then entry.setup() end
    end
  end
end

return M
