local M = {}

function M.setup()
  local map = require("core.keymaps").map

  local ok, spectre = pcall(require, "spectre")
  if ok then
    spectre.setup({})

    map("n", "<leader>sr", function()
      spectre.toggle()
    end, "Replace in files")

    map("n", "<leader>sW", function()
      spectre.open_visual({ select_word = true })
    end, "Replace current word")

    map("v", "<leader>sW", function()
      spectre.open_visual()
    end, "Replace selection")

    map("n", "<leader>sF", function()
      spectre.open_file_search({ select_word = true })
    end, "Replace in current file")
  end
end

return M
