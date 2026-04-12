local M = {}

---@param mode string|string[]
---@param lhs string
---@param rhs string|function
---@param desc string
---@param opts table|nil
function M.map(mode, lhs, rhs, desc, opts)
  opts = opts or {}
  opts.desc = desc
  opts.silent = opts.silent ~= false
  vim.keymap.set(mode, lhs, rhs, opts)
end

function M.setup()
  local map = M.map

  map({ "n", "i", "v" }, "<C-s>", function()
    vim.cmd.write()
  end, "Save file")

  map("n", "<leader>qq", "<cmd>qa<cr>", "Quit all")
  map("n", "<leader>qw", "<cmd>wq<cr>", "Save and quit")
  map("n", "<leader>bd", "<cmd>bdelete<cr>", "Delete buffer")
  map("n", "<leader>bn", "<cmd>bnext<cr>", "Next buffer")
  map("n", "<leader>bp", "<cmd>bprevious<cr>", "Previous buffer")

  map("n", "<C-h>", "<C-w>h", "Focus left window")
  map("n", "<C-j>", "<C-w>j", "Focus lower window")
  map("n", "<C-k>", "<C-w>k", "Focus upper window")
  map("n", "<C-l>", "<C-w>l", "Focus right window")

  map("n", "<leader>wh", "<C-w>h", "Focus left window")
  map("n", "<leader>wj", "<C-w>j", "Focus lower window")
  map("n", "<leader>wk", "<C-w>k", "Focus upper window")
  map("n", "<leader>wl", "<C-w>l", "Focus right window")
  map("n", "<leader>wv", "<C-w>v", "Split vertically")
  map("n", "<leader>ws", "<C-w>s", "Split horizontally")
  map("n", "<leader>wq", "<C-w>q", "Close window")
  map("n", "<leader>w=", "<C-w>=", "Balance windows")
  map("n", "<leader>uf", "<cmd>FrontendModeCycle<cr>", "Cycle frontend mode")
  map("n", "<leader>uF", "<cmd>FrontendMode<cr>", "Show frontend mode")
  map("n", "<leader>ut", "<cmd>ThemeCycle<cr>", "Cycle theme")
  map("n", "<leader>uT", "<cmd>Theme<cr>", "Show theme")

  map("n", "<Esc>", "<cmd>nohlsearch<cr>", "Clear search highlight")
  map("t", "<Esc><Esc>", "<C-\\><C-n>", "Exit terminal mode")
  map("v", "<", "<gv", "Indent left")
  map("v", ">", ">gv", "Indent right")

  local user_keymaps = require("core.util").require_if_exists("user.keymaps")
  if type(user_keymaps) == "function" then user_keymaps(M) end
end

return M
