local neotest = pRequire("neotest")
local cfg = require("pisces").config.neotest

if neotest and cfg then
  -- get neotest namespace (api call creates or returns namespace)
  local neotest_ns = vim.api.nvim_create_namespace("neotest")
  vim.diagnostic.config({
    virtual_text = {
      format = function(diagnostic)
        local message = diagnostic.message:gsub("\n", " "):gsub("\t", " "):gsub("%s+", " "):gsub("^%s+", "")
        return message
      end,
    },
  }, neotest_ns)
  ----------------------------------------------------------------
  neotest.setup({
    adapters = require("pisces.env").getNeotestAdapters(),

    summary = {
      animated = true,
      enabled = true,
      expand_errors = true,
      follow = true,
      mappings = {
        expand = { "o", "<2-LeftMouse>" },
        jumpto = "<CR>",
        output = "O",
        short = "s",
      },
    },

    icons = {
      passed = "",
      failed = "",
      running = "",
      running_animated = { "▫", "▪" },
      skipped = "-",
      unknown = "",
    },
    diagnostic = {
      enabled = true,
    },
    status = {
      signs = false,
      virtual_text = true,
    },
  })

  keymap("n", cfg.toggle, function()
    neotest.summary.toggle()
  end, {desc = 'Neotest Toggle'})

  keymap("n", cfg.run, function()
    neotest.run.run()
  end, { desc = 'Neotest Run'})

  keymap("n", cfg.run_dap, function()
    neotest.run.run({ strategy = "dap" })
  end, { desc = 'Neotest Dap Run'})

  keymap("n", cfg.run_file, function()
    neotest.run.run(vim.fn.expand("%"))
  end, { desc = 'Neotest Run File'})

  keymap("n", cfg.run_stop, function()
    neotest.run.stop()
  end, { desc = 'Neotest Stop'})

  keymap("n", cfg.output_open, function()
    neotest.output.open({ enter = true })
  end, { desc = 'Neotest Output'})
end
