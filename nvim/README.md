# Neovim Configuration

This directory is the active Neovim config. It has been rebuilt around native Neovim 0.12+ features and `vim.pack`, with a stable override layer under `lua/user/`.

The old AstroNvim / Lazy / preset-based structure is no longer the source of truth. If you want to extend or override behavior, use [docs/customization.md](/d:/config/dotfiles/nvim/docs/customization.md).

Historical notes such as `MIGRATION.md`, `LEGACY-FILES.md`, and `ENHANCEMENT_GUIDE.md` are archival only. The files in this README are the current source of truth.

## Goals

- Prefer built-in Neovim features first
- Keep modern plugins where they clearly improve UX
- Keep keymaps predictable and grouped
- Keep personal overrides isolated from the base config
- Support frontend, backend, markdown, and terminal-heavy workflows without bringing back a framework layer

## Current Stack

Core:

- Native `vim.pack` plugin management
- Native `vim.lsp.config()` and `vim.lsp.enable()`
- Mason for tool and LSP installation
- Treesitter for syntax and structure
- Conform for non-frontend formatting

UI and navigation:

- `snacks.nvim` for dashboard, picker, explorer, notifier, and git search
- `flash.nvim` for motion
- `nvim-spider` for smarter `w/e/b/ge` word motions
- `noice.nvim` for message and commandline UI
- `blink.cmp` for completion
- `bufferline.nvim` for buffer tabs
- `which-key.nvim` for grouped keymap hints
- `nvim-spectre` for interactive search and replace

Themes:

- `tokyonight`
- `catppuccin`
- `kanagawa`

If a preferred theme is missing or broken locally, the config now falls back to a theme that actually exists on disk.

## Language Support

LSP is configured directly in [lua/lsp/servers.lua](/d:/config/dotfiles/nvim/lua/lsp/servers.lua).

Built-in base support includes:

- Lua: `lua_ls`
- Bash: `bashls`
- JSON: `jsonls`
- YAML: `yamlls`
- TOML: `taplo`
- Markdown: `marksman`
- Go: `gopls`
- Rust: `rust_analyzer`
- Zig: `zls`
- Python: `ruff`, optional `ty`
- CSS / HTML: `cssls`, `emmet_language_server`
- JavaScript / TypeScript / React: `vtsls`, `eslint`
- Vue: `vue_ls` with `@vue/typescript-plugin` wired into `vtsls`

Frontend formatting defaults to ESLint-driven workflows so projects using `@antfu/eslint-config` can keep one source of truth. Non-frontend filetypes continue to use Conform.

## Useful Commands

Config and status:

- `:NvimConfig` open the config directory
- `:NvimUserConfig` open the user override directory
- `:NvimStatus` show active config path, data path, current theme, and frontend mode

Theme:

- `:Theme` show current theme
- `:Theme <name>` switch theme for the current session
- `:ThemeCycle` rotate bundled themes

Frontend mode:

- `:FrontendMode` show current frontend mode
- `:FrontendMode eslint`
- `:FrontendMode eslint_imports`
- `:FrontendMode conform`
- `:FrontendModeCycle`

Plugin management:

- `:PackUpdate` open a fuzzy picker of installed plugins
- `:PackUpdate <plugin>` update one plugin
- `:PackUpdate!` update all plugins
- `:PackStatus` inspect plugin status with the same picker
- `:PackRepair` show only broken or missing plugins
- `:PackRepair <plugin>` reinstall one broken plugin
- `:PackRepair!` repair all broken or missing plugins
- `:PackSync` write the current resolved state to [nvim-pack-lock.json](/d:/config/dotfiles/nvim/nvim-pack-lock.json)
- `:PackOpenLog` open the lightweight `vim.pack` activity log
- `:MarkdownTableFormat` format the markdown table under cursor

`PackRepair` is especially useful when a plugin directory is corrupted and only contains `.git`.

## Keymap Shape

Global:

- `<leader><space>` smart file picker
- `<leader>,` buffers
- `<leader>/` grep project
- `<leader>e` explorer
- `<C-s>` save
- `<leader>qq` quit all

Search and picker:

- `<leader>ff` files
- `<leader>fg` git files
- `<leader>fr` recent files
- `<leader>fc` config files
- `<leader>sb` buffer lines
- `<leader>sd` diagnostics
- `<leader>sh` help
- `<leader>sk` keymaps
- `<leader>sn` notifications
- `<leader>sw` grep current word with `snacks.picker`
- `<leader>sr` replace in files with `nvim-spectre`
- `<leader>sW` replace current word or selection with `nvim-spectre`
- `<leader>sF` replace in current file with `nvim-spectre`
- `<leader>sR` resume last picker

Theme and frontend runtime toggles:

- `<leader>uf` cycle frontend mode
- `<leader>uF` show frontend mode
- `<leader>ut` cycle theme
- `<leader>uT` show theme
- `<leader>uC` colorscheme picker
- `<leader>gg` LazyGit through `snacks.nvim`

Workflow and tools:

- `<leader>gj` Jujutsu TUI
- `<leader>js` Jujutsu status
- `<leader>jl` Jujutsu log
- `<leader>jd` Jujutsu diff
- `<leader>ld` LazyDocker
- `<leader>ls` Live Server
- `<leader>db` toggle DBUI

Plugin maintenance:

- `<leader>pu` plugin update picker
- `<leader>pP` update all plugins
- `<leader>ps` plugin status picker
- `<leader>pr` plugin repair picker
- `<leader>pl` pack log

LSP and code navigation:

- `K` hover
- `gd` definitions
- `gD` declarations
- `gr` references
- `gI` implementations
- `gy` type definitions
- `<leader>ca` code action
- `<leader>cr` rename
- `<leader>xd` line diagnostics
- `w/e/b/ge` smart subword motions via `nvim-spider`

Filetype-local actions use `<localleader>`.

Examples:

- Markdown: `<localleader>m*`
- `package.json`: `<localleader>n*`
- Hurl: `<localleader>h*`
- Frontend buffers: `<localleader>l*`, `<localleader>t*`, `<localleader>v*`

## Structure

```text
nvim/
├── init.lua
├── nvim-pack-lock.json
├── after/
│   └── ftplugin/
├── docs/
├── lua/
│   ├── core/      # options, diagnostics, keymaps, commands, terminal helpers
│   ├── lang/      # runtime language modes, python helper logic
│   ├── lsp/       # LSP registry and setup
│   ├── pack/      # vim.pack specs, commands, and UI
│   ├── plugins/   # plugin setup grouped by responsibility
│   └── user/      # stable override entrypoints
```

Main runtime chain:

1. [init.lua](/d:/config/dotfiles/nvim/init.lua)
2. [lua/core/](/d:/config/dotfiles/nvim/lua/core)
3. [lua/pack/](/d:/config/dotfiles/nvim/lua/pack)
4. [lua/plugins/](/d:/config/dotfiles/nvim/lua/plugins)
5. [lua/lsp/](/d:/config/dotfiles/nvim/lua/lsp)

## User Overrides

The base config is meant to stay stable. Put personal changes under [lua/user/](/d:/config/dotfiles/nvim/lua/user).

Entry points:

- [lua/user/init.lua](/d:/config/dotfiles/nvim/lua/user/init.lua) theme, extra plugins, disabled base plugins, extra LSP config
- [lua/user/options.lua](/d:/config/dotfiles/nvim/lua/user/options.lua) option overrides
- [lua/user/keymaps.lua](/d:/config/dotfiles/nvim/lua/user/keymaps.lua) extra keymaps
- [lua/user/autocmds.lua](/d:/config/dotfiles/nvim/lua/user/autocmds.lua) extra autocmds
- [lua/user/commands.lua](/d:/config/dotfiles/nvim/lua/user/commands.lua) custom commands
- [lua/user/lang.lua](/d:/config/dotfiles/nvim/lua/user/lang.lua) treesitter, formatters, frontend mode defaults, extra Mason tools
- [lua/user/plugins/](/d:/config/dotfiles/nvim/lua/user/plugins) extra plugins
- [lua/user/lsp/](/d:/config/dotfiles/nvim/lua/user/lsp) extra or overridden LSP servers
- [after/ftplugin/](/d:/config/dotfiles/nvim/after/ftplugin) per-filetype local behavior

See [docs/customization.md](/d:/config/dotfiles/nvim/docs/customization.md) for examples.

## Installation

Requirements:

- Neovim 0.12 or newer
- `git`
- Nerd Font recommended
- External tools as needed for your languages, for example `ruff`, `stylua`, `goimports`, `gofumpt`, `shfmt`

Typical install:

Windows:

```powershell
Move-Item $env:LOCALAPPDATA\nvim $env:LOCALAPPDATA\nvim.bak -ErrorAction SilentlyContinue
git clone <repository-url> $env:LOCALAPPDATA\nvim
nvim
```

Linux / macOS:

```bash
mv ~/.config/nvim ~/.config/nvim.bak 2>/dev/null
git clone <repository-url> ~/.config/nvim
nvim
```

On first launch, `vim.pack` will offer to install configured plugins.

## Troubleshooting

Theme fails to load:

- Run `:PackStatus` or `:PackUpdate`
- If the plugin directory is broken, run `:PackRepair <plugin>`
- Restart Neovim after plugin changes

`PackRepair` says a plugin is active:

- Restart once with `NVIM_NO_PLUGINS=1`
- Run `:PackRepair <plugin>` in that session

Headless checks fail in a restricted sandbox:

- `vim.pack` bootstrap needs `git` on PATH
- Neovim also needs write access to its data directory for logs and ShaDa
- If you validate startup in a sandboxed environment, these errors may be environmental rather than config issues

Frontend save behavior is too aggressive:

- Use `:FrontendMode conform`
- Or adjust [lua/user/lang.lua](/d:/config/dotfiles/nvim/lua/user/lang.lua)

Need a quick health snapshot:

- `:NvimStatus`
- `:checkhealth`
- `:messages`

## Notes From `cap153-nvim`

Most framework-level ideas from `cap153-nvim` were intentionally not carried forward. The current config already absorbed the parts that aged well:

- `snacks.nvim` as the main dashboard and picker layer
- `blink.cmp`
- `noice.nvim`
- direct Neovim 0.11+ / 0.12+ LSP style instead of framework glue
- the older custom dashboard header

Still potentially useful as future borrow candidates:

- optional GUI-specific helpers for Neovide

Already borrowed into the new base:

- markdown table auto-formatting with a manual `:MarkdownTableFormat` command
- a few picker interaction details such as `<Esc>`, `<C-e>`, and `<C-u>`
- `snacks.nvim` lazygit integration

Those are treated as optional enhancements, not core architecture.
