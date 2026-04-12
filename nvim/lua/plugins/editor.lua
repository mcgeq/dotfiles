local M = {}

function M.setup()
  local map = require("core.keymaps").map
  local user_lang = require("core.util").require_if_exists("user.lang") or {}
  local frontend_state = require("lang.frontend")
  local eslint_filetypes = {
    css = true,
    html = true,
    javascript = true,
    javascriptreact = true,
    less = true,
    scss = true,
    typescript = true,
    typescriptreact = true,
    vue = true,
    json = true,
    jsonc = true,
    yaml = true,
    toml = true,
  }
  local function current_frontend()
    return frontend_state.get()
  end

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

  local function frontend_owns_format(bufnr)
    local frontend = current_frontend()
    for _, filetype in ipairs(frontend.eslint_filetypes or {}) do
      eslint_filetypes[filetype] = true
    end
    return frontend.formatter ~= "conform" and eslint_filetypes[vim.bo[bufnr].filetype]
  end

  local ok_gitsigns, gitsigns = pcall(require, "gitsigns")
  if ok_gitsigns then
    gitsigns.setup()
    map("n", "]h", gitsigns.next_hunk, "Next hunk")
    map("n", "[h", gitsigns.prev_hunk, "Previous hunk")
    map("n", "<leader>ghs", gitsigns.stage_hunk, "Stage hunk")
    map("n", "<leader>ghr", gitsigns.reset_hunk, "Reset hunk")
    map("n", "<leader>ghp", gitsigns.preview_hunk, "Preview hunk")
    map("n", "<leader>ghb", function() gitsigns.blame_line({ full = true }) end, "Blame line")
  end

  local ok_blink, blink = pcall(require, "blink.cmp")
  if ok_blink then
    blink.setup({
      appearance = {
        nerd_font_variant = "mono",
      },
      completion = {
        documentation = { auto_show = true },
        list = { selection = { preselect = false, auto_insert = false } },
      },
      fuzzy = {
        prebuilt_binaries = {
          force_version = "v*",
        },
      },
      keymap = {
        preset = "default",
        ["<C-space>"] = { "show", "show_documentation", "hide_documentation" },
      },
      snippets = {
        preset = "default",
      },
      sources = {
        default = { "lsp", "path", "snippets", "buffer" },
      },
    })
  end

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

  local ok_conform, conform = pcall(require, "conform")
  if ok_conform then
    local formatters_by_ft = {
      go = { "gofumpt", "goimports" },
      lua = { "stylua" },
      python = { "ruff_fix", "ruff_format" },
      sh = { "shfmt" },
    }
    formatters_by_ft = vim.tbl_deep_extend("force", formatters_by_ft, user_lang.formatters_by_ft or {})

    local formatters = {
      ruff_fix = {
        command = "ruff",
        args = { "check", "--fix", "--stdin-filename", "$FILENAME" },
        stdin = true,
      },
      ruff_format = {
        command = "ruff",
        args = { "format", "--stdin-filename", "$FILENAME" },
        stdin = true,
      },
    }
    formatters = vim.tbl_deep_extend("force", formatters, user_lang.formatters or {})

    conform.setup({
      format_on_save = function(bufnr)
        if frontend_owns_format(bufnr) then return end
        return {
          timeout_ms = 1000,
          lsp_format = "fallback",
        }
      end,
      formatters_by_ft = formatters_by_ft,
      formatters = formatters,
    })

    map({ "n", "v" }, "<leader>cf", function()
      local bufnr = vim.api.nvim_get_current_buf()
      if frontend_owns_format(bufnr) and vim.fn.exists(":EslintFixAll") == 2 then
        pcall(vim.cmd, "silent EslintFixAll")
        return
      end
      conform.format({ async = true })
    end, "Format buffer")
  end

  local ok_mason, mason = pcall(require, "mason")
  if ok_mason then
    mason.setup({
      ui = {
        border = "rounded",
      },
    })
  end

  local ok_tool_installer, tool_installer = pcall(require, "mason-tool-installer")
  if ok_tool_installer then
    local tools = {
      "gofumpt",
      "goimports",
      "ruff",
      "shfmt",
      "stylua",
    }
    vim.list_extend(tools, user_lang.mason or {})
    tools = vim.fn.uniq(vim.fn.sort(tools))

    tool_installer.setup({
      ensure_installed = tools,
      run_on_start = false,
      auto_update = false,
    })
  end
end

return M
