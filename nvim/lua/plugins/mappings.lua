return {
  {
    "AstroNvim/astrocore",
    ---@type AstroCoreOpts
    opts = {
      mappings = {
        -- Normal 模式 Ctrl+S 保存
        n = {
          ["<C-s>"] = { ":w!<CR>", desc = "Save File" },
        },
        -- Insert 模式 Ctrl+S 保存
        i = {
          ["<C-s>"] = { "<Esc>:w!<CR>", desc = "Save File" },
        },
      },
    },
  },
}