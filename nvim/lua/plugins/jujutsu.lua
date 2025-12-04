-- Jujutsu VCS 配置
-- lazyjj - Jujutsu 的 TUI 工具（类似 lazygit）

---@type LazySpec
return {
  {
    "AstroNvim/astrocore",
    opts = {
      mappings = {
        n = {
          -- Jujutsu TUI（需要先安装 lazyjj: cargo install lazyjj）
          ["<Leader>gj"] = {
            function()
              local Terminal = require("toggleterm.terminal").Terminal
              local lazyjj = Terminal:new {
                cmd = "lazyjj",
                hidden = true,
                direction = "float",
                float_opts = {
                  border = "curved",
                  width = function() return math.floor(vim.o.columns * 0.9) end,
                  height = function() return math.floor(vim.o.lines * 0.9) end,
                },
                on_open = function(term)
                  vim.cmd "startinsert!"
                  vim.api.nvim_buf_set_keymap(
                    term.bufnr,
                    "n",
                    "q",
                    "<cmd>close<CR>",
                    { noremap = true, silent = true }
                  )
                end,
              }
              lazyjj:toggle()
            end,
            desc = "LazyJJ (Jujutsu TUI)",
          },

          -- Jujutsu 命令快捷键（可选）
          ["<Leader>jj"] = { ":!jj ", desc = "Run jj command" },
          ["<Leader>js"] = { ":!jj status<CR>", desc = "JJ Status" },
          ["<Leader>jl"] = { ":!jj log<CR>", desc = "JJ Log" },
          ["<Leader>jd"] = { ":!jj diff<CR>", desc = "JJ Diff" },
        },
      },
    },
  },

  -- 可选：安装 vim-jujutsu 插件以获得更好的集成
  -- {
  --   "ejpcmac/vim-jujutsu",
  --   ft = "jj", -- 仅在 jj 文件类型时加载
  -- },
}
