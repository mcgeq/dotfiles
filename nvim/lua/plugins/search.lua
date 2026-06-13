local M = {}

function M.setup()
  local map = require("core.keymaps").map
  local loader = require("pack.loader")
  local spectre_ready = false

  local function spectre()
    if not loader.ensure("nvim-spectre") then return end
    local ok, module = pcall(require, "spectre")
    if not ok then return end
    if not spectre_ready then
      module.setup({})
      spectre_ready = true
    end
    return module
  end

  map("n", "<leader>sr", function()
    local module = spectre()
    if module then module.toggle() end
  end, "Replace in files")

  map("n", "<leader>su", function()
    local module = spectre()
    if module then module.resume_last_search() end
  end, "Resume replace search")

  map("n", "<leader>sQ", function()
    local module = spectre()
    if not module then return end
    local ok_actions, actions = pcall(require, "spectre.actions")
    if ok_actions and actions and actions.send_to_qf then
      actions.send_to_qf()
    end
  end, "Send replace results to quickfix")

  map("n", "<leader>sW", function()
    local module = spectre()
    if module then module.open_visual({ select_word = true }) end
  end, "Replace current word")

  map("v", "<leader>sW", function()
    local module = spectre()
    if module then module.open_visual() end
  end, "Replace selection")

  map("n", "<leader>sF", function()
    local module = spectre()
    if module then module.open_file_search({ select_word = true }) end
  end, "Replace in current file")

  map("v", "<leader>sF", function()
    local module = spectre()
    if module then module.open_file_search() end
  end, "Replace selection in current file")
end

return M
