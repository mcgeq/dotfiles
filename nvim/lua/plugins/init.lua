local M = {}

function M.setup()
  local util = require("core.util")

  require("plugins.ui").setup()
  require("plugins.noice").setup()
  require("plugins.snacks").setup()
  require("plugins.flash").setup()
  require("plugins.editor").setup()
  require("plugins.backend").setup()
  require("plugins.frontend").setup()
  require("plugins.search").setup()
  require("plugins.web").setup()
  require("plugins.workflow").setup()
  require("plugins.markdown").setup()

  for _, entry in ipairs(util.load_table_modules("user.plugins")) do
    if type(entry.setup) == "function" then entry.setup() end
  end
end

return M
