local M = {}

function M.setup()
  local map = require("core.keymaps").map

  local ok_pairs, mini_pairs = pcall(require, "mini.pairs")
  if ok_pairs then
    mini_pairs.setup()
  end

  local ok_ai, mini_ai = pcall(require, "mini.ai")
  if ok_ai then mini_ai.setup() end

  local ok_surround, mini_surround = pcall(require, "mini.surround")
  if ok_surround then mini_surround.setup() end

  local ok_comment, mini_comment = pcall(require, "mini.comment")
  if ok_comment then mini_comment.setup() end

  local ok_spider, spider = pcall(require, "spider")
  if ok_spider then
    spider.setup({
      skipInsignificantPunctuation = true,
      subwordMovement = true,
      consistentOperatorPending = false,
    })

    -- Use ex-commands so dot-repeat keeps working in operator-pending mode.
    map({ "n", "o", "x" }, "w", "<cmd>lua require('spider').motion('w')<cr>", "Spider forward word")
    map({ "n", "o", "x" }, "e", "<cmd>lua require('spider').motion('e')<cr>", "Spider end word")
    map({ "n", "o", "x" }, "b", "<cmd>lua require('spider').motion('b')<cr>", "Spider backward word")
    map({ "n", "o", "x" }, "ge", "<cmd>lua require('spider').motion('ge')<cr>", "Spider backward end word")
  end
end

return M
