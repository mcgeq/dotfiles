local M = {}

function M.setup()
  local ok, flash = pcall(require, "flash")
  if not ok then return end

  flash.setup({})

  local map = require("core.keymaps").map
  map({ "n", "x", "o" }, "s", function() flash.jump() end, "Flash jump")
  map({ "n", "x", "o" }, "S", function() flash.treesitter() end, "Flash treesitter")
  map("o", "r", function() flash.remote() end, "Remote flash")
  map({ "o", "x" }, "R", function() flash.treesitter_search() end, "Treesitter search")
  map("c", "<C-s>", function() flash.toggle() end, "Toggle flash search")

  local ok_snacks, snacks = pcall(require, "snacks")
  if ok_snacks and snacks.config and snacks.config.picker then
    local picker = snacks.config.picker
    picker.actions = picker.actions or {}
    picker.actions.flash = function(picker_instance)
      flash.jump({
        pattern = "^",
        label = { after = { 0, 0 } },
        search = {
          mode = "search",
          exclude = {
            function(win)
              return vim.bo[vim.api.nvim_win_get_buf(win)].filetype ~= "snacks_picker_list"
            end,
          },
        },
        action = function(match)
          local idx = picker_instance.list:row2idx(match.pos[1])
          picker_instance.list:_move(idx, true, true)
        end,
      })
    end
    picker.win = picker.win or {}
    picker.win.input = picker.win.input or {}
    picker.win.input.keys = picker.win.input.keys or {}
    picker.win.input.keys["<A-s>"] = { "flash", mode = { "n", "i" } }
    picker.win.input.keys["s"] = { "flash", mode = { "n" } }
  end
end

return M
