local M = {}

function M.setup()
  local map = require("core.keymaps").map
  local user_lang = require("user").lang

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

  local ok_treesitter, treesitter = pcall(require, "nvim-treesitter")

  -- Treesitter syntax highlighting and indentation (Neovim 0.10+ native)
  local ts_groups = vim.api.nvim_create_augroup("plugins_treesitter_native", { clear = true })
  vim.api.nvim_create_autocmd("FileType", {
    group = ts_groups,
    pattern = "*",
    callback = function(ev)
      if vim.fn.exists "##TreesitterStart" == 0 and not vim.b[ev.buf].ts_highlight then
        pcall(vim.treesitter.start, ev.buf)
      end
    end,
  })

  -- Treesitter-based folding (Neovim 0.10+ native foldexpr)
  local fold_groups = vim.api.nvim_create_augroup("plugins_treesitter_fold", { clear = true })
  vim.api.nvim_create_autocmd("FileType", {
    group = fold_groups,
    pattern = "*",
    callback = function()
      local ok_parser, parser = pcall(vim.treesitter.get_parser, 0)
      if ok_parser and parser then
        vim.opt_local.foldmethod = "expr"
        vim.opt_local.foldexpr = "v:lua.vim.treesitter.foldexpr()"
      end
    end,
  })

  local ok_textobjects, textobjects = pcall(require, "nvim-treesitter-textobjects")
  if ok_textobjects then
    textobjects.setup {
      select = {
        lookahead = true,
        selection_modes = {
          ["@parameter.outer"] = "v",
          ["@function.outer"] = "V",
          ["@class.outer"] = "V",
        },
      },
      move = {
        set_jumps = true,
      },
    }

    local ts_select = require "nvim-treesitter-textobjects.select"
    local ts_move = require "nvim-treesitter-textobjects.move"
    local ts_swap = require "nvim-treesitter-textobjects.swap"

    -- select: af/if functions, ac/ic classes, ab/ib blocks
    map(
      { "x", "o" },
      "af",
      function() ts_select.select_textobject("@function.outer", "textobjects") end,
      "Select around function"
    )
    map(
      { "x", "o" },
      "if",
      function() ts_select.select_textobject("@function.inner", "textobjects") end,
      "Select inside function"
    )
    map(
      { "x", "o" },
      "ac",
      function() ts_select.select_textobject("@class.outer", "textobjects") end,
      "Select around class"
    )
    map(
      { "x", "o" },
      "ic",
      function() ts_select.select_textobject("@class.inner", "textobjects") end,
      "Select inside class"
    )
    map(
      { "x", "o" },
      "ab",
      function() ts_select.select_textobject("@block.outer", "textobjects") end,
      "Select around block"
    )
    map(
      { "x", "o" },
      "ib",
      function() ts_select.select_textobject("@block.inner", "textobjects") end,
      "Select inside block"
    )

    -- move: jump between functions/classes/blocks
    map(
      { "n", "x", "o" },
      "]f",
      function() ts_move.goto_next_start("@function.outer", "textobjects") end,
      "Next function start"
    )
    map(
      { "n", "x", "o" },
      "]F",
      function() ts_move.goto_next_end("@function.outer", "textobjects") end,
      "Next function end"
    )
    map(
      { "n", "x", "o" },
      "[f",
      function() ts_move.goto_previous_start("@function.outer", "textobjects") end,
      "Previous function start"
    )
    map(
      { "n", "x", "o" },
      "[F",
      function() ts_move.goto_previous_end("@function.outer", "textobjects") end,
      "Previous function end"
    )
    map(
      { "n", "x", "o" },
      "]c",
      function() ts_move.goto_next_start("@class.outer", "textobjects") end,
      "Next class start"
    )
    map(
      { "n", "x", "o" },
      "[c",
      function() ts_move.goto_previous_start("@class.outer", "textobjects") end,
      "Previous class start"
    )

    -- swap: swap parameters
    map("n", ">a", function() ts_swap.swap_next "@parameter.inner" end, "Swap next parameter")
    map("n", "<a", function() ts_swap.swap_previous "@parameter.inner" end, "Swap previous parameter")
  end

  -- Install missing parsers asynchronously on startup
  if ok_treesitter and parsers and #parsers > 0 then
    local installed = treesitter.get_installed()
    local missing = vim.tbl_filter(function(p) return not vim.list_contains(installed, p) end, parsers)
    if #missing > 0 then treesitter.install(missing, { force = true, summary = true }) end
  end

  local ok_context, context = pcall(require, "treesitter-context")
  if ok_context then
    context.setup {
      enable = true,
      max_lines = 3,
      mode = "cursor",
      trim_scope = "outer",
    }
  end

  local ok_todo, todo = pcall(require, "todo-comments")
  if ok_todo then
    todo.setup {}
    map("n", "]t", function() todo.jump_next() end, "Next todo comment")
    map("n", "[t", function() todo.jump_prev() end, "Previous todo comment")
    map("n", "<leader>st", function()
      if vim.fn.exists ":TodoTrouble" == 2 then
        vim.cmd.TodoTrouble()
      elseif vim.fn.exists ":TodoQuickFix" == 2 then
        vim.cmd.TodoQuickFix()
      else
        vim.cmd [[vimgrep /TODO\|FIXME\|HACK\|NOTE/ **/*]]
        vim.cmd.copen()
      end
    end, "Todo comments")
  end

  local ok_trouble, trouble = pcall(require, "trouble")
  if ok_trouble then
    trouble.setup {}
    map("n", "<leader>xx", "<cmd>Trouble diagnostics toggle<cr>", "Diagnostics list")
    map("n", "<leader>xX", "<cmd>Trouble diagnostics toggle filter.buf=0<cr>", "Buffer diagnostics list")
    map("n", "<leader>xQ", "<cmd>Trouble qflist toggle<cr>", "Quickfix list")
    map("n", "<leader>xL", "<cmd>Trouble loclist toggle<cr>", "Location list")
    map("n", "<leader>cs", "<cmd>Trouble symbols toggle focus=false<cr>", "Document symbols")
  end
end

return M
