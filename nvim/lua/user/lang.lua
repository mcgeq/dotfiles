return {
  -- Frontend formatting strategy.
  -- formatter = "eslint" uses EslintFixAll for JS/TS/Vue/JSON/YAML/TOML on save.
  -- Set formatter = "conform" if you prefer filetype formatters below instead.
  frontend = {
    formatter = "eslint",
    eslint_fix_on_save = true,
    -- Session modes are available via :FrontendMode and :FrontendModeCycle.
    save_actions = {
      eslint = true,
      organize_imports = false,
      remove_unused = false,
      add_missing_imports = false,
    },
    -- Add more filetypes here if you also want ESLint to own formatting.
    eslint_filetypes = {},
  },

  -- Markdown extras borrowed from the old cap153-nvim workflow.
  markdown = {
    table = {
      format_on_insert_leave = true,
      format_on_pipe = true,
      notify_on_manual_format_failure = true,
    },
  },

  -- Add extra treesitter parsers here.
  treesitter = {},

  -- Add or override conform formatters_by_ft entries here.
  -- Example: switch Vue/CSS/HTML back to prettier without touching the base config.
  -- formatters_by_ft = {
  --   vue = { "prettier" },
  --   css = { "prettier" },
  --   html = { "prettier" },
  -- },
  formatters_by_ft = {},

  -- Add custom conform formatter definitions here.
  -- Example:
  -- formatters = {
  --   prettier = {
  --     command = "prettier",
  --     args = { "--stdin-filepath", "$FILENAME" },
  --     stdin = true,
  --   },
  -- },
  formatters = {},

  -- Add extra Mason tools here, for example: { "prettier" }.
  mason = {},
}
