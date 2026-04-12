local M = {}

function M.setup()
  local map = require("core.keymaps").map
  local terminal = require("core.terminal")

  map("n", "<leader>gj", function()
    if vim.fn.executable("lazyjj") == 1 then
      terminal.open_float("lazyjj", { title = " lazyjj " })
    else
      terminal.open_float("jj log", { title = " jj log " })
    end
  end, "Open Jujutsu TUI")

  map("n", "<leader>js", function() terminal.open_float("jj status", { title = " jj status " }) end, "Jujutsu status")
  map("n", "<leader>jl", function() terminal.open_float("jj log", { title = " jj log " }) end, "Jujutsu log")
  map("n", "<leader>jd", function() terminal.open_float("jj diff", { title = " jj diff " }) end, "Jujutsu diff")
  map("n", "<leader>ld", function() terminal.open_float("lazydocker", { title = " lazydocker " }) end, "LazyDocker")
end

return M
