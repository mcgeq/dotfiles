local M = {}

local workspace = require "core.workspace"
local buffers = require "core.keymaps.buffers"
local windows = require "core.keymaps.windows"
local quickfix = require "core.keymaps.quickfix"
local editing = require "core.keymaps.editing"

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
  local sessions = require "core.sessions"

  map({ "n", "i", "v" }, "<C-s>", function() vim.cmd.write() end, "Save file")

  map("n", "<leader>qq", "<cmd>qa<cr>", "Quit all")
  map("n", "<leader>qw", "<cmd>wq<cr>", "Save and quit")
  map("n", "<leader>bb", buffers.pick, "Pick buffer")
  map("n", "<leader>bd", buffers.close_current, "Delete buffer")
  map("n", "<leader>bo", buffers.close_other, "Delete other buffers")
  map("n", "<leader>bP", buffers.close_project, "Delete project buffers")
  map("n", "<leader><tab>", buffers.alternate, "Alternate buffer")
  map("n", "<leader>bh", buffers.close_left, "Delete left buffers")
  map("n", "<leader>bl", buffers.close_right, "Delete right buffers")
  map("n", "<leader>bn", "<cmd>bnext<cr>", "Next buffer")
  map("n", "<leader>bp", "<cmd>bprevious<cr>", "Previous buffer")
  map(
    "n",
    "<leader>bt",
    buffers.bufferline_command("BufferLineTogglePin", function() buffers.notify_bufferline_feature "Buffer pinning" end),
    "Toggle buffer pin"
  )
  map(
    "n",
    "<leader>bH",
    buffers.bufferline_command(
      "BufferLineMovePrev",
      function() buffers.notify_bufferline_feature "Buffer reordering" end
    ),
    "Move buffer left"
  )
  map(
    "n",
    "<leader>bL",
    buffers.bufferline_command(
      "BufferLineMoveNext",
      function() buffers.notify_bufferline_feature "Buffer reordering" end
    ),
    "Move buffer right"
  )

  for index = 1, 9 do
    map("n", ("<leader>b%d"):format(index), buffers.goto_buffer(index), ("Go to buffer %d"):format(index))
  end

  map("n", "<C-h>", "<C-w>h", "Focus left window")
  map("n", "<C-j>", "<C-w>j", "Focus lower window")
  map("n", "<C-k>", "<C-w>k", "Focus upper window")
  map("n", "<C-l>", "<C-w>l", "Focus right window")

  map("n", "<leader>wm", windows.toggle_maximize, "Maximize window")
  map("n", "<leader>wo", windows.close_other_windows, "Close other windows")
  map("n", "<leader>wv", "<C-w>v", "Split vertically")
  map("n", "<leader>ws", "<C-w>s", "Split horizontally")
  map("n", "<leader>wq", "<C-w>q", "Close window")
  map("n", "<leader>wT", windows.move_window_to_new_tab, "Move window to new tab")
  map("n", "<leader>w=", "<C-w>=", "Balance windows")
  map("n", "<leader>tn", windows.new_tab, "New tab")
  map("n", "<leader>th", windows.previous_tab, "Previous tab")
  map("n", "<leader>tl", windows.next_tab, "Next tab")
  map("n", "<leader>tq", windows.close_tab, "Close tab")
  map("n", "<leader>to", windows.close_other_tabs, "Close other tabs")
  map("n", "[q", quickfix.quickfix_prev, "Previous quickfix item")
  map("n", "]q", quickfix.quickfix_next, "Next quickfix item")
  map("n", "[l", quickfix.loclist_prev, "Previous location list item")
  map("n", "]l", quickfix.loclist_next, "Next location list item")
  map("n", "<leader>xc", quickfix.clear_quickfix, "Clear quickfix list")
  map("n", "<leader>xC", quickfix.clear_loclist, "Clear location list")
  map("n", "<leader>od", editing.duplicate_line_below, "Duplicate line below")
  map("n", "<leader>os", editing.open_scratch, "Open scratch buffer")
  map("n", "<leader>nn", "<cmd>NvimStatus<cr>", "Nvim status")
  map("n", "<leader>nc", "<cmd>NvimConfig<cr>", "Open nvim config")
  map("n", "<leader>nC", "<cmd>NvimUserConfig<cr>", "Open user config")
  map("n", "<leader>nh", "<cmd>Health<cr>", "Run health check")
  map("n", "<leader>ni", "<cmd>LspInfo<cr>", "LSP info")
  map("n", "<leader>nl", "<cmd>LspLog<cr>", "LSP log")
  map("n", "<leader>nL", "<cmd>LspLogRaw<cr>", "Raw LSP log")
  map("n", "<leader>nr", "<cmd>RunnerKeys<cr>", "Runner key summary")
  map("n", "<leader>nm", "<cmd>Mason<cr>", "Open Mason")
  map("n", "<leader>nR", "<cmd>LspRestart<cr>", "Restart buffer LSP")
  map("n", "<leader>nM", "<cmd>messages<cr>", "Show messages")
  map("n", "<leader>mi", sessions.show_current, "Session info")
  map("n", "<leader>ms", sessions.save_current, "Save workspace session")
  map("n", "<leader>ml", sessions.load_current, "Load workspace session")
  map("n", "<leader>mS", function() sessions.select "read" end, "Select workspace session")
  map("n", "<leader>md", sessions.delete_current, "Delete workspace session")
  map("n", "<leader>mD", function() sessions.select "delete" end, "Select session to delete")
  map("n", "<leader>mr", sessions.restart, "Restart with workspace session")
  map("n", "<leader>Pi", workspace.show_info, "Project info")
  map("n", "<leader>Pr", workspace.cd_to_current_root, "Set cwd to current project root")
  map("n", "<leader>Pp", workspace.select, "Select project")
  map("n", "<leader>PS", function() workspace.select { load_session = true } end, "Select project + session")
  map("n", "<leader>uf", "<cmd>FrontendModeCycle<cr>", "Cycle frontend mode")
  map("n", "<leader>uF", "<cmd>FrontendMode<cr>", "Show frontend mode")
  map("n", "<leader>ut", "<cmd>ThemeCycle<cr>", "Cycle theme")
  map("n", "<leader>uT", "<cmd>Theme<cr>", "Show theme")

  map("n", "<Esc>", "<cmd>nohlsearch<cr>", "Clear search highlight")
  map("t", "<Esc><Esc>", "<C-\\><C-n>", "Exit terminal mode")
  map("n", "<A-j>", editing.move_line_down, "Move line down")
  map("n", "<A-k>", editing.move_line_up, "Move line up")
  map("i", "<A-j>", "<Esc><cmd>move +1<cr>==gi", "Move line down")
  map("i", "<A-k>", "<Esc><cmd>move -2<cr>==gi", "Move line up")
  map("v", "<A-j>", ":move '>+1<cr>gv=gv", "Move selection down")
  map("v", "<A-k>", ":move '<-2<cr>gv=gv", "Move selection up")
  map("v", "<leader>od", "y`>p`[v`]", "Duplicate selection below", { silent = true })
  map("v", "<", "<gv", "Indent left")
  map("v", ">", ">gv", "Indent right")

  local user_keymaps = require("core.util").require_if_exists "user.keymaps"
  if type(user_keymaps) == "function" then user_keymaps(M) end
end

return M
