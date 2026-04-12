local M = {}

function M.setup()
  local map = require("core.keymaps").map

  local ok_go, go = pcall(require, "go")
  if ok_go then
    go.setup({
      fillstruct = "gopls",
      lsp_cfg = false,
      dap_debug = false,
    })

    vim.api.nvim_create_autocmd("FileType", {
      group = vim.api.nvim_create_augroup("plugins_go_keymaps", { clear = true }),
      pattern = { "go", "gomod" },
      callback = function(event)
        map("n", "<localleader>fs", "<cmd>GoFillStruct<cr>", "Fill struct", { buffer = event.buf })
        map("n", "<localleader>ie", "<cmd>GoIfErr<cr>", "Add if err", { buffer = event.buf })
        map("n", "<localleader>at", "<cmd>GoAddTag<cr>", "Add tags", { buffer = event.buf })
        map("n", "<localleader>rt", "<cmd>GoRmTag<cr>", "Remove tags", { buffer = event.buf })
        map("n", "<localleader>im", "<cmd>GoImpl<cr>", "Generate implementation", { buffer = event.buf })
        map("n", "<localleader>tf", "<cmd>GoTestFunc<cr>", "Test function", { buffer = event.buf })
        map("n", "<localleader>ta", "<cmd>GoTest<cr>", "Test package", { buffer = event.buf })
        map("n", "<localleader>tc", "<cmd>GoCoverage<cr>", "Test coverage", { buffer = event.buf })
      end,
    })
  end

  vim.g.db_ui_use_nerd_fonts = 1
  vim.g.db_ui_win_position = "right"
  vim.g.db_ui_winwidth = 40
  vim.g.db_ui_show_database_icon = 1
  vim.g.db_ui_force_echo_notifications = 1
  vim.g.db_ui_save_location = vim.fn.stdpath("data") .. "/db_ui"

  map("n", "<leader>db", "<cmd>DBUIToggle<cr>", "Toggle database UI")
  map("n", "<leader>df", "<cmd>DBUIFindBuffer<cr>", "Find database buffer")
  map("n", "<leader>dr", "<cmd>DBUIRenameBuffer<cr>", "Rename database buffer")
  map("n", "<leader>dq", "<cmd>DBUILastQueryInfo<cr>", "Database last query")
end

return M
