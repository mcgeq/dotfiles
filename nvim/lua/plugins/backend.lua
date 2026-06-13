local M = {}

function M.setup()
  local map = require("core.keymaps").map
  local loader = require("pack.loader")
  local runner = require("lang.runner")

  runner.setup()

  map("n", "<leader>ri", function() runner.show_keys(vim.api.nvim_get_current_buf()) end, "Runner info")
  map("n", "<leader>rb", function() runner.run("build", vim.api.nvim_get_current_buf()) end, "Runner build")
  map("n", "<leader>rr", function() runner.run("run", vim.api.nvim_get_current_buf()) end, "Runner run")
  map("n", "<leader>rt", function() runner.run("test", vim.api.nvim_get_current_buf()) end, "Runner test")
  map("n", "<leader>rT", function() runner.run("test_file", vim.api.nvim_get_current_buf()) end, "Runner test file")
  map("n", "<leader>rl", function() runner.run("repeat_last", vim.api.nvim_get_current_buf()) end, "Runner repeat")
  map("n", "<leader>rc", function() runner.run("clean", vim.api.nvim_get_current_buf()) end, "Runner clean")

  local go_ready = false
  local function ensure_go()
    if go_ready then return true end
    if not loader.ensure_many({ "guihua.lua", "go.nvim" }) then return false end
    local ok_go, go = pcall(require, "go")
    if not ok_go then return false end
    go.setup({
      fillstruct = "gopls",
      lsp_cfg = false,
      dap_debug = false,
    })
    go_ready = true
    return true
  end

  vim.api.nvim_create_autocmd("FileType", {
    group = vim.api.nvim_create_augroup("plugins_go_keymaps", { clear = true }),
    pattern = { "go", "gomod" },
    callback = function(event)
      ensure_go()
      if go_ready then
        map("n", "<localleader>fs", "<cmd>GoFillStruct<cr>", "Fill struct", { buffer = event.buf })
        map("n", "<localleader>ie", "<cmd>GoIfErr<cr>", "Add if err", { buffer = event.buf })
        map("n", "<localleader>at", "<cmd>GoAddTag<cr>", "Add tags", { buffer = event.buf })
        map("n", "<localleader>aT", "<cmd>GoRmTag<cr>", "Remove tags", { buffer = event.buf })
        map("n", "<localleader>im", "<cmd>GoImpl<cr>", "Generate implementation", { buffer = event.buf })
        map("n", "<localleader>tf", "<cmd>GoTestFunc<cr>", "Test function", { buffer = event.buf })
        map("n", "<localleader>ta", "<cmd>GoTest<cr>", "Test package", { buffer = event.buf })
        map("n", "<localleader>tc", "<cmd>GoCoverage<cr>", "Test coverage", { buffer = event.buf })
      end
    end,
  })

  vim.g.db_ui_use_nerd_fonts = 1
  vim.g.db_ui_win_position = "right"
  vim.g.db_ui_winwidth = 40
  vim.g.db_ui_show_database_icon = 1
  vim.g.db_ui_force_echo_notifications = 1
  vim.g.db_ui_save_location = vim.fn.stdpath("data") .. "/db_ui"

  local function with_dbui(command)
    return function()
      if not loader.ensure_many({ "vim-dadbod", "vim-dadbod-completion", "vim-dadbod-ui" }) then return end
      vim.cmd(command)
    end
  end

  map("n", "<leader>db", with_dbui("DBUIToggle"), "Toggle database UI")
  map("n", "<leader>df", with_dbui("DBUIFindBuffer"), "Find database buffer")
  map("n", "<leader>dr", with_dbui("DBUIRenameBuffer"), "Rename database buffer")
  map("n", "<leader>dq", with_dbui("DBUILastQueryInfo"), "Database last query")
end

return M
