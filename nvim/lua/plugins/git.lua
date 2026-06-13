local M = {}

function M.setup()
  local map = require("core.keymaps").map
  local ok_gitsigns, gitsigns = pcall(require, "gitsigns")
  if not ok_gitsigns then return end

  gitsigns.setup()
  map("n", "]h", gitsigns.next_hunk, "Next hunk")
  map("n", "[h", gitsigns.prev_hunk, "Previous hunk")
  map("n", "<leader>ghs", gitsigns.stage_hunk, "Stage hunk")
  map("n", "<leader>ghr", gitsigns.reset_hunk, "Reset hunk")
  map("n", "<leader>ghp", gitsigns.preview_hunk, "Preview hunk")
  map("n", "<leader>ghb", function() gitsigns.blame_line({ full = true }) end, "Blame line")
  map("n", "<leader>ghq", function() gitsigns.setqflist("attached", { open = true }) end, "File hunks to quickfix")
  map("n", "<leader>ghl", function() gitsigns.setloclist(0, "attached") end, "File hunks to location list")
  map("n", "<leader>ghS", gitsigns.stage_buffer, "Stage buffer")
  map("n", "<leader>ghu", gitsigns.undo_stage_hunk, "Undo stage hunk")
  map("n", "<leader>ghD", gitsigns.diffthis, "Diff buffer")
end

return M
