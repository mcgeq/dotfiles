local dap = pRequire("dap")
local dapui = pRequire("dapui")
local M = {}
if dap and dapui then
  local cfg = require("pisces").config.dap

  if cfg then
    M.keyAttach = function()
    -- run
    keymap("n", cfg.continue, dap.continue, { desc = 'DAP Continue' })
    -- nnoremap <silent> <Leader>dl <Cmd>lua require'dap'.run_last()<CR>

    --  stepOver, stepInto, stepOut,
    keymap("n", cfg.step_over, dap.step_over,  { desc = 'DAP StepOver' })
    keymap("n", cfg.step_into, dap.step_into, { desc = 'DAP SetpInto' })
    keymap("n", cfg.step_out, dap.step_out, { desc = 'DAP StepOut' })

    keymap("n", cfg.toggle_breakpoint, dap.toggle_breakpoint, { desc = 'DAP Toggle Breakpoint' })
    -- nnoremap <silent> <Leader>B <Cmd>lua require'dap'.set_breakpoint(vim.fn.input('Breakpoint condition: '))<CR>
    -- nnoremap <silent> <Leader>lp <Cmd>lua require'dap'.set_breakpoint(nil, nil, vim.fn.input('Log point message: '))<CR>
    keymap("n", cfg.clear_breakpoints, dap.clear_breakpoints, { desc = 'DAP Clear Breakpoint'})

    keymap("n", cfg.eval, dapui.eval, { desc = 'DAP Eval'})
    -- nnoremap <silent> <Leader>dr <Cmd>lua require'dap'.repl.open()<CR>

    keymap("n", cfg.terminate, dap.terminate, { desc = 'DAP Terminate' })
    -- keymap(
    --   "n",
    --   "<leader>de",
    --   ":lua require'dap'.close()<CR>"
    --     .. ":lua require'dap'.terminate()<CR>"
    --     .. ":lua require'dap.repl'.close()<CR>"
    --     .. ":lua require'dapui'.close()<CR>"
    --     .. ":lua require('dap').clear_breakpoints()<CR>"
    --     .. "<C-w>o<CR>"
    -- )

    -- rust
    -- map("n", "<leader>dd", ":RustDebuggables<CR>", opt)
    end
  end
end

return M
