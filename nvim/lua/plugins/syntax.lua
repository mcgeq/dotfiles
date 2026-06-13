local M = {}

function M.setup()
  local map = require("core.keymaps").map
  local user_lang = require("core.util").require_if_exists("user.lang") or {}

  local ok_treesitter, treesitter = pcall(require, "nvim-treesitter.configs")
  if ok_treesitter then
    local parsers = {
      "bash",
      "comment",
      "css",
      "dockerfile",
      "git_config",
      "gitcommit",
      "gitignore",
      "git_rebase",
      "go",
      "html",
      "javascript",
      "json",
      "jsonc",
      "lua",
      "markdown",
      "markdown_inline",
      "python",
      "query",
      "regex",
      "rust",
      "scss",
      "toml",
      "tsx",
      "typescript",
      "vim",
      "vimdoc",
      "vue",
      "yaml",
      "zig",
    }
    vim.list_extend(parsers, user_lang.treesitter or {})

    treesitter.setup({
      ensure_installed = parsers,
      auto_install = true,
      highlight = {
        enable = true,
        additional_vim_regex_highlighting = false,
      },
      indent = { enable = true },
      incremental_selection = {
        enable = true,
        keymaps = {
          init_selection = "<C-space>",
          node_incremental = "<C-space>",
          scope_incremental = false,
          node_decremental = "<bs>",
        },
      },
    })
  end

  local ok_context, context = pcall(require, "treesitter-context")
  if ok_context then
    context.setup({
      enable = true,
      max_lines = 3,
      mode = "cursor",
      trim_scope = "outer",
    })
  end

  local ok_todo, todo = pcall(require, "todo-comments")
  if ok_todo then
    todo.setup({})
    map("n", "]t", function() todo.jump_next() end, "Next todo comment")
    map("n", "[t", function() todo.jump_prev() end, "Previous todo comment")
    map("n", "<leader>st", function()
      if vim.fn.exists(":TodoTrouble") == 2 then
        vim.cmd.TodoTrouble()
      elseif vim.fn.exists(":TodoQuickFix") == 2 then
        vim.cmd.TodoQuickFix()
      else
        vim.cmd([[vimgrep /TODO\|FIXME\|HACK\|NOTE/ **/*]])
        vim.cmd.copen()
      end
    end, "Todo comments")
  end

  local ok_trouble, trouble = pcall(require, "trouble")
  if ok_trouble then
    trouble.setup({})
    map("n", "<leader>xx", "<cmd>Trouble diagnostics toggle<cr>", "Diagnostics list")
    map("n", "<leader>xX", "<cmd>Trouble diagnostics toggle filter.buf=0<cr>", "Buffer diagnostics list")
    map("n", "<leader>xQ", "<cmd>Trouble qflist toggle<cr>", "Quickfix list")
    map("n", "<leader>xL", "<cmd>Trouble loclist toggle<cr>", "Location list")
    map("n", "<leader>cs", "<cmd>Trouble symbols toggle focus=false<cr>", "Document symbols")
  end
end

return M
