# User Customization

The base configuration is designed to stay stable. Put personal changes in `lua/user/` so future refactors do not require editing core files.

## Common entry points

- `lua/user/init.lua`: colorscheme override, extra plugins, disabled base plugins, extra LSP servers
- `lua/user/options.lua`: option overrides
- `lua/user/keymaps.lua`: extra keymaps
- `lua/user/autocmds.lua`: extra autocmds
- `lua/user/commands.lua`: extra user commands
- `lua/user/lang.lua`: extra treesitter parsers, conform formatters, Mason tools
- `lua/user/plugins/*.lua`: one file per extra plugin or plugin group
- `lua/user/lsp/*.lua`: one file per extra language server
- `after/ftplugin/*.lua`: per-filetype local behavior

## Default keymap shape

- global search and replace uses `<leader>s*`
- filetype-specific actions use `<localleader>`
- examples:
  - Markdown: `<localleader>m*`
  - `package.json`: `<localleader>n*`
  - Hurl: `<localleader>h*`
  - Go: `<localleader>f/i/a/r/t*`

## Add a plugin

Create `lua/user/plugins/gitlinker.lua`:

```lua
return {
  spec = {
    src = "https://github.com/linrongbin16/gitlinker.nvim",
    name = "gitlinker.nvim",
  },
  setup = function()
    require("gitlinker").setup()
  end,
}
```

## Add a language server

Create `lua/user/lsp/clangd.lua`:

```lua
return {
  name = "clangd",
  ensure_installed = true,
  config = {},
}
```

## Extend language support

Edit `lua/user/lang.lua`:

```lua
return {
  frontend = {
    formatter = "eslint",
    eslint_fix_on_save = true,
    save_actions = {
      eslint = true,
      organize_imports = false,
      remove_unused = false,
      add_missing_imports = false,
    },
  },
  treesitter = { "sql" },
  mason = { "sqls" },
  formatters_by_ft = {
    sql = { "sqlfluff" },
  },
}
```

Markdown table behavior also lives in `lua/user/lang.lua`:

```lua
return {
  markdown = {
    table = {
      format_on_insert_leave = true,
      format_on_pipe = true,
      notify_on_manual_format_failure = false,
    },
  },
}
```

## Switch Frontend Formatting

The base config defaults to ESLint-driven formatting for JS, TS, React, Vue, JSON, YAML, and TOML so projects using `@antfu/eslint-config` can keep one formatting source of truth.

Save actions are conservative by default so one save does not trigger repeated overlapping edits:

- ESLint fix on save: on
- organize imports on save: off
- remove unused on save: off
- add missing imports on save: off

If you want to switch some filetypes back to `conform` plus your own formatter definitions, edit `lua/user/lang.lua`:

```lua
return {
  frontend = {
    formatter = "conform",
  },
  mason = { "prettier" },
  formatters_by_ft = {
    vue = { "prettier" },
    css = { "prettier" },
    html = { "prettier" },
  },
  formatters = {
    prettier = {
      command = "prettier",
      args = { "--stdin-filepath", "$FILENAME" },
      stdin = true,
    },
  },
}
```

If you want a stronger TS workflow on save, enable it explicitly:

```lua
return {
  frontend = {
    formatter = "eslint",
    save_actions = {
      eslint = true,
      organize_imports = true,
      remove_unused = false,
      add_missing_imports = false,
    },
  },
}
```

You can also switch the active frontend mode at runtime:

- `:FrontendMode`: show current mode
- `:FrontendMode eslint`: ESLint-only mode
- `:FrontendMode eslint_imports`: ESLint plus organize imports
- `:FrontendMode conform`: hand frontend files back to `conform`
- `:FrontendModeCycle`: rotate modes for the current session

Default keymaps:

- `<leader>uf`: cycle frontend mode
- `<leader>uF`: show current frontend mode

## Frontend Localleader

For `javascript`, `typescript`, `react`, and `vue` buffers:

- `<localleader>lf`: ESLint fix all
- `<localleader>li`: organize imports
- `<localleader>lm`: add missing imports
- `<localleader>lu`: remove unused code
- `<localleader>lv`: code actions
- `<localleader>ls`: show attached frontend LSP clients
- `<localleader>tf`: TS/JS fix all
- `<localleader>ti`: TS/JS organize imports
- `<localleader>tm`: TS/JS add missing imports
- `<localleader>tu`: TS/JS remove unused
- `<localleader>vf`: Vue fix all
- `<localleader>vi`: Vue organize imports
- `<localleader>vm`: Vue add missing imports
- `<localleader>vu`: Vue remove unused
- `<localleader>vs`: Vue/TS client status
- `<localleader>va`: Vue code actions

## Markdown Localleader

- `<localleader>mf`: format the current markdown table
- `<localleader>mt`: toggle `vim-table-mode`
- `<localleader>mi`: paste markdown image
- `<localleader>mT`: generate Markdown TOC

Markdown tables now auto-align on `InsertLeave`, and can also auto-align right after typing `|`.

## Pack install UI

- `vim.pack.add()` does not ship with a full plugin-manager dashboard.
- `vim.pack.update()` provides a built-in confirmation view.
- This config adds lightweight progress notifications plus `:PackOpenLog` using `PackChangedPre` and `PackChanged`.
- `:PackUpdate` now opens a fuzzy picker of installed plugins when called without arguments.
- `:PackUpdate <plugin>` updates one specific plugin directly.
- `:PackUpdate!` updates all installed plugins directly.
- `:PackStatus` now opens the same fuzzy picker for offline status checks.
- `:PackRepair` opens a picker with only broken or missing plugins.
- `:PackRepair <plugin>` repairs one plugin by removing its broken checkout and reinstalling it.
- `:PackRepair!` repairs all broken or missing plugins directly.
- Picker entries show `status + plugin name + revision + source`.
- Status markers:
- `[A]`: active in the current session
- `[I]`: installed on disk but not active in the current session
- `[!]`: broken worktree on disk, usually a plugin directory that only has `.git`
- `[?]`: missing plugin path
- `:PackSync` only writes the current resolved state back to `nvim-pack-lock.json`.
- `:PackRepair` is the quickest fix when a plugin directory is corrupted on disk.
- `<leader>pu`: open plugin update picker
- `<leader>pP`: update all plugins
- `<leader>pr`: open repair picker
- `<leader>ps`: open plugin status picker

Snacks picker input extras:

- `<Esc>`: close picker immediately
- `<C-e>`: move down in the result list
- `<C-u>`: move up in the result list

## Add keymaps

Edit `lua/user/keymaps.lua`:

```lua
return function(map)
  map.map("n", "<leader>um", "<cmd>Mason<cr>", "Open Mason")
end
```

Vue / TS example:

```lua
return function(map)
  vim.api.nvim_create_autocmd("FileType", {
    pattern = { "vue", "typescript", "typescriptreact" },
    callback = function(event)
      map.map("n", "<localleader>vo", "<cmd>FrontendMode eslint_imports<cr>", "Vue/TS imports mode", { buffer = event.buf })
    end,
  })
end
```

## Theme Overrides

The base config ships with `tokyonight`, `catppuccin`, and `kanagawa`.

Edit `lua/user/init.lua`:

```lua
local M = {
  theme = {
    name = "kanagawa",
    overrides = {
      kanagawa = {
        theme = "dragon",
        transparent = true,
      },
    },
    highlights = {
      CursorLineNr = { bold = true },
      NormalFloat = { bg = "NONE" },
    },
  },
}

return M
```

`colorscheme = "tokyonight"` is still supported for backward compatibility, but `theme.name` is the preferred entry point now.

Runtime commands:

- `:Theme`: show current theme
- `:Theme kanagawa`: switch theme for the current session
- `:ThemeCycle`: cycle bundled themes
- `<leader>ut`: cycle bundled themes
- `<leader>uT`: show current theme
