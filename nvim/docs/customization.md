# User Customization

The base configuration is designed to stay stable. Put personal changes in `lua/user/` so future refactors do not require editing core files.

## Where to put changes

| Need | Preferred location | Why |
| --- | --- | --- |
| Global personal overrides | `lua/user/init.lua` plus focused modules like `lua/user/theme.lua` | Keeps top-level preferences out of the base runtime without forcing unrelated settings into one file |
| Extra plugins | `lua/user/plugins/*.lua` | Keeps plugin additions isolated and removable |
| LSP additions or overrides | `lua/user/lsp/*.lua` | Keeps server-specific changes close to the server they affect |
| Filetype-local buffer behavior | `after/ftplugin/*.lua` | Best place for narrow local behavior and editor options |
| Shared behavior across buffers or plugins | Base domain modules such as `lua/lang/*` or `lua/plugins/*` | Use only when behavior is meant to be part of the base config, not a personal tweak |

## Common entry points

- `lua/user/init.lua`: thin entrypoint that merges focused user modules
- `lua/user/theme.lua`: colorscheme override and highlight tweaks
- `lua/user/pack.lua`: disable base plugins or add small `vim.pack` specs
- `lua/user/session.lua`: session behavior toggles
- `lua/user/lsp_settings.lua`: Mason-backed LSP install toggles
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
- buffer actions use `<leader>b*`
- git hunk actions use `<leader>gh*`
- small editing helpers use `<leader>o*`
- workspace sessions use `<leader>m*`
- project switching and root helpers use `<leader>P*`
- global run and test helpers use `<leader>r*`
- filetype-specific actions use `<localleader>`
- examples:
  - Buffers: `<leader>bb/bd/bo/bP/bh/bl/bn/bp/bt/bH/bL/b1...b9`
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

## LSP extension path

- Add new servers or overrides in `lua/user/lsp/*.lua`.
- Use `lsp.ensure_installed = { ... }` in `lua/user/lsp_settings.lua` only to request Mason-backed installs.
- Keep full server configs out of `lua/user/init.lua` and `lua/user/lsp_settings.lua`.

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

## Runner Localleader

For Python, frontend, Rust, Zig, and Go buffers:

- `<localleader>rb`: build
- `<localleader>rr`: run
- `<localleader>rt`: run the default test command
- `<localleader>rl`: repeat the last runner command

Extra support where available:

- `<localleader>rT`: run tests for the current file in Python and supported frontend projects
- `:RunnerKeys`: show the active runner keymap summary

Python and frontend file-scoped tests send output to quickfix so failures are easier to jump through.

## Global Runner Shortcuts

You can also drive the current buffer's runner from global `<leader>r*` mappings:

- `<leader>ri`: show runner info for the current buffer
- `<leader>rb`: build the current project or buffer target
- `<leader>rr`: run the current project or buffer target
- `<leader>rt`: run the current project or package test target
- `<leader>rT`: run tests for the current file when supported
- `<leader>rl`: repeat the last runner command
- `<leader>rc`: clean the current project or buffer target

Matching commands are also available:

- `:RunnerInfo`
- `:RunnerBuild`
- `:RunnerRun`
- `:RunnerTest`
- `:RunnerTestFile`
- `:RunnerRepeat`
- `:RunnerClean`

## Runtime Shortcuts

Daily runtime and maintenance commands live under `<leader>n*`:

- `<leader>nn`: show Neovim runtime status
- `<leader>nc`: open the main config directory
- `<leader>nC`: open the user override directory
- `<leader>nh`: run the asynchronous health check
- `<leader>ni`: show current-buffer LSP info
- `<leader>nl`: open the latest-first LSP log view
- `<leader>nL`: open the raw LSP log file
- `<leader>nR`: restart attached LSP clients for the current buffer
- `<leader>nr`: show runner keymaps for the current filetype
- `<leader>nm`: open Mason
- `<leader>nM`: show `:messages`

## Workspace Sessions

Workspace session helpers live under `<leader>m*`:

- `<leader>mi`: show the current workspace session target
- `<leader>ms`: save the current workspace session
- `<leader>ml`: load the current workspace session
- `<leader>mS`: select and load a saved workspace session
- `<leader>md`: delete the current workspace session
- `<leader>mD`: select and delete a saved workspace session
- `<leader>mr`: restart Neovim and restore the current workspace session

Matching commands are also available:

- `:SessionInfo`
- `:SessionSave`
- `:SessionLoad`
- `:SessionSelect`
- `:SessionDelete`
- `:SessionSelectDelete`
- `:SessionRestart`

Sessions are stored under `stdpath("data") .. "/session"` by default and named from the workspace root path, so different projects do not collide.

## Project Switching And Roots

Project and root helpers live under `<leader>P*`:

- `<leader>Pi`: show cwd, detected project root, buffer root, and session status
- `<leader>Pr`: set the current working directory to the current buffer project root
- `<leader>Pp`: pick a known project and switch cwd
- `<leader>PS`: pick a known project, switch cwd, and load its saved session if one exists

Matching commands are also available:

- `:ProjectInfo`
- `:ProjectRoot`
- `:ProjectSelect`
- `:ProjectSessionSelect`

Known projects are gathered conservatively from the current cwd, open buffer roots, and saved workspace sessions, so the picker stays useful without pulling in another project plugin.

## Terminal Workflow

General shell helpers live under `<leader>l*`:

- `<leader>lt`: open a shell in the current working directory
- `<leader>lp`: open a shell in the current project root
- `<leader>lb`: open a shell in the current buffer directory
- `<leader>lT`: reopen the last floating terminal command

Matching commands are also available:

- `:TerminalCwd`
- `:TerminalProject`
- `:TerminalBufferDir`
- `:TerminalLast`

## Markdown Localleader

- `<localleader>mf`: format the current markdown table
- `<localleader>mr`: toggle in-buffer Markdown rendering
- `<localleader>mp`: open a rendered Markdown preview to the side
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

## Diagnostics And Debugging

Diagnostics can be viewed inline, in floats, through Snacks pickers, or in Trouble:

- `<leader>xd`: line diagnostics
- `<leader>xx`: workspace diagnostics
- `<leader>xX`: current buffer diagnostics
- `<leader>xq`: send diagnostics to quickfix
- `<leader>xl`: send diagnostics to the current window location list
- `<leader>xc`: clear quickfix
- `<leader>xC`: clear location list
- `<leader>xQ`: quickfix list
- `<leader>xL`: location list
- `[d` / `]d`: previous or next diagnostic
- `[e` / `]e`: previous or next error
- `[w` / `]w`: previous or next warning

Debugging uses `nvim-dap`:

- Python uses `debugpy`, preferring `uv run python -m debugpy.adapter`
- C, C++, Rust, and Zig use `codelldb` when available
- Debug keymaps use `<leader>D*` so they do not conflict with the database UI under `<leader>d*`

Common debug keys:

- `<leader>Dc`: continue / launch
- `<leader>Db`: toggle breakpoint
- `<leader>Do`: step over
- `<leader>Di`: step into
- `<leader>Du`: toggle DAP UI

Snacks picker input extras:

- `<Esc>`: close picker immediately
- `<C-e>`: move down in the result list
- `<C-u>`: move up in the result list

## Add keymaps

Edit `lua/user/keymaps.lua`:

```lua
return function(map)
  map.map("n", "<leader>nt", "<cmd>tabnew<cr>", "Scratch tab")
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

Edit `lua/user/theme.lua`:

```lua
local M = {
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
