[中文](README.zh.md)

# Neovim Configuration

This directory is the active Neovim config. It has been rebuilt around native Neovim 0.12+ features and `vim.pack`, with a stable override layer under `lua/user/`.

The old AstroNvim / Lazy / preset-based structure is no longer the source of truth. If you want to extend or override behavior, use [docs/customization.md](/d:/config/dotfiles/nvim/docs/customization.md).

Historical notes such as `MIGRATION.md`, `LEGACY-FILES.md`, and `ENHANCEMENT_GUIDE.md` are archival only. The files in this README are the current source of truth.

## Maintenance Contract

- [README.md](/d:/config/dotfiles/nvim/README.md) describes active behavior, supported commands, and the current extension model.
- [docs/customization.md](/d:/config/dotfiles/nvim/docs/customization.md) shows where personal changes should live.
- [docs/architecture.md](/d:/config/dotfiles/nvim/docs/architecture.md) maps module ownership and runtime flow.
- `MIGRATION.md`, `LEGACY-FILES.md`, and `ENHANCEMENT_GUIDE.md` are archive material for migration history only.

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
- Heavy optional plugins are installed by `vim.pack`, but several workflow-specific tools now load on demand through `:packadd`

UI and navigation:

- `snacks.nvim` for dashboard, picker, explorer, notifier, and git search
- `flash.nvim` for motion
- `nvim-spider` for smarter `w/e/b/ge` word motions
- `noice.nvim` for message and commandline UI
- `blink.cmp` for completion
- `bufferline.nvim` for buffer tabs
- `which-key.nvim` for grouped keymap hints
- `nvim-spectre` for interactive search and replace
- `trouble.nvim` for diagnostics, symbols, quickfix, and location lists
- `todo-comments.nvim` for TODO / FIXME navigation
- `nvim-treesitter-context` for lightweight code context

Themes:

- `tokyonight`
- `catppuccin`
- `kanagawa`

If a preferred theme is missing or broken locally, the config now falls back to a theme that actually exists on disk.

## Language Support

LSP is configured directly in [lua/lsp/servers.lua](/d:/config/dotfiles/nvim/lua/lsp/servers.lua).

Built-in base support includes:

- C / C++: `clangd` via [lua/user/lsp/clangd.lua](/d:/config/dotfiles/nvim/lua/user/lsp/clangd.lua)
- Lua: `lua_ls`
- Bash: `bashls`
- JSON: `jsonls` with SchemaStore schemas
- YAML: `yamlls` with SchemaStore schemas
- TOML: `taplo`
- Markdown: `marksman`
- Go: `gopls`
- Rust: `rust_analyzer`
- Zig: `zls`
- Python: `ruff` plus `uv + ty` by default, falling back to `basedpyright` when `ty` is not installed
- CSS / HTML: `cssls`, `emmet_language_server`
- JavaScript / TypeScript / React: `vtsls`, `eslint`
- Vue: `vue_ls` with `@vue/typescript-plugin` wired into `vtsls`

Frontend formatting defaults to ESLint-driven workflows so projects using `@antfu/eslint-config` can keep one source of truth. Non-frontend filetypes continue to use Conform.

For C++, the user override prefers a system LLVM toolchain when available and falls back to Mason's `clangd`. See [docs/cpp-clangd.md](/d:/config/dotfiles/nvim/docs/cpp-clangd.md) for C++20/23 defaults, `query-driver`, and project-level module setup.

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
- `:RenderMarkdown toggle` toggle in-buffer Markdown rendering
- `:RenderMarkdown preview` open a rendered Markdown preview to the side

Terminal:

- `:TerminalCwd` open a floating shell in the current working directory
- `:TerminalProject` open a floating shell in the current project root
- `:TerminalBufferDir` open a floating shell in the current buffer directory
- `:TerminalLast` reopen the last floating terminal command

Workspace sessions:

- `:SessionInfo` show the session target for the current workspace
- `:SessionSave` save the current workspace session
- `:SessionLoad` load the current workspace session
- `:SessionSelect` pick and load a saved workspace session
- `:SessionDelete` delete the current workspace session
- `:SessionSelectDelete` pick and delete a saved workspace session
- `:SessionRestart` restart Neovim and restore the current workspace session

Projects and roots:

- `:ProjectInfo` show cwd, detected project root, current buffer root, and session status
- `:ProjectRoot` set the current working directory to the current buffer project root
- `:ProjectSelect` pick a known project and switch cwd
- `:ProjectSessionSelect` pick a known project, switch cwd, and load its saved session if present

Runner:

- `:RunnerInfo` show run/test actions for the current buffer
- `:RunnerBuild` build the current project or buffer target
- `:RunnerRun` run the current project or buffer target
- `:RunnerTest` run the current project or buffer test target
- `:RunnerTestFile` run tests for the current file when supported
- `:RunnerRepeat` repeat the last runner command
- `:RunnerClean` clean the current project or buffer target

C++ / CMake extras:

- `clangd_extensions.nvim` adds source/header switching plus AST, symbol, hierarchy, and memory views
- `cmake-tools.nvim` adds in-editor generate / build / run workflows for CMake projects
- `nvim-dap`, `dap-ui`, and `dap-virtual-text` provide a debugging baseline for Python plus C/C++/Rust/Zig through `debugpy` and `codelldb`

`PackRepair` is especially useful when a plugin directory is corrupted and only contains `.git`.

## Keymap Shape

Global:

- `<leader><space>` smart file picker
- `<leader>,` buffers
- `<leader>/` grep project
- `<leader>e` explorer
- `<C-s>` save
- `<leader>qq` quit all

Buffer:

- `<leader>bb` pick buffer
- `<leader><Tab>` switch to alternate buffer
- `<leader>bd` delete current buffer
- `<leader>bo` delete other buffers
- `<leader>bP` delete other buffers in the current project
- `<leader>bh` delete buffers to the left
- `<leader>bl` delete buffers to the right
- `<leader>bn` next buffer
- `<leader>bp` previous buffer
- `<leader>bt` toggle pin for current buffer
- `<leader>bH` move current buffer left
- `<leader>bL` move current buffer right
- `<leader>b1` ... `<leader>b9` go to buffer by position

Window:

- `<C-h/j/k/l>` focus window by direction
- `<leader>wm` maximize or restore current window
- `<leader>wo` close other windows
- `<leader>wv` split vertically
- `<leader>ws` split horizontally
- `<leader>wq` close window
- `<leader>wT` move current window to a new tab
- `<leader>w=` balance windows

Tab:

- `<leader>tn` new tab
- `<leader>th` previous tab
- `<leader>tl` next tab
- `<leader>tq` close current tab
- `<leader>to` close other tabs

Editing:

- `<A-j>` / `<A-k>` move the current line or selection down or up
- `<leader>od` duplicate the current line below
- visual `<leader>od` duplicate the current selection below
- `<leader>os` open a scratch buffer

Lists and jumps:

- `[q` / `]q` previous or next quickfix item
- `[l` / `]l` previous or next location list item
- `[d` / `]d` previous or next diagnostic
- `[e` / `]e` previous or next error
- `[w` / `]w` previous or next warning
- `[t` / `]t` previous or next TODO / FIXME comment

Search and picker:

- `<leader>ff` files
- `<leader>fg` git files
- `<leader>fr` recent files
- `<leader>fc` config files
- `<leader>sb` buffer lines
- `<leader>sd` project diagnostics
- `<leader>sD` buffer diagnostics
- `<leader>ss` LSP symbols
- `<leader>sS` workspace symbols
- `<leader>sm` marks
- `<leader>sc` commands
- `<leader>sp` plugin files
- `<leader>st` TODO / FIXME comments
- `<leader>sh` help
- `<leader>sk` keymaps
- `<leader>sn` notifications
- `<leader>sw` grep current word or visual selection with `snacks.picker`
- `<leader>sr` replace in files with `nvim-spectre`
- `<leader>su` resume the last `nvim-spectre` search
- `<leader>sQ` send `nvim-spectre` results to quickfix
- `<leader>sW` replace current word or selection with `nvim-spectre`
- `<leader>sF` replace in current file with `nvim-spectre`
- `<leader>sR` resume last picker

Theme and frontend runtime toggles:

- `<leader>nn` show Neovim runtime status
- `<leader>nc` open the main config directory
- `<leader>nC` open the user override directory
- `<leader>nh` run the asynchronous health check
- `<leader>ni` show current-buffer LSP info
- `<leader>nl` open the latest-first LSP log view
- `<leader>nL` open the raw LSP log file
- `<leader>nR` restart LSP clients for the current buffer
- `<leader>nr` show runner keymaps for the current filetype
- `<leader>nm` open Mason
- `<leader>nM` show `:messages`
- `<leader>uf` cycle frontend mode
- `<leader>uF` show frontend mode
- `<leader>ut` cycle theme
- `<leader>uT` show theme
- `<leader>uC` colorscheme picker
- `<leader>gg` LazyGit through `snacks.nvim`
- `<leader>gb` Git branches
- `<leader>gs` Git status
- `<leader>gl` Git log
- `<leader>gf` Git file log
- `<leader>gL` Git line log

Workspace sessions:

- `<leader>mi` show the current workspace session target
- `<leader>ms` save the current workspace session
- `<leader>ml` load the current workspace session
- `<leader>mS` select and load a saved workspace session
- `<leader>md` delete the current workspace session
- `<leader>mD` select and delete a saved workspace session
- `<leader>mr` restart Neovim with the current workspace session restored

Projects and roots:

- `<leader>Pi` show current project and root info
- `<leader>Pr` set cwd to the current buffer project root
- `<leader>Pp` select a known project
- `<leader>PS` select a known project and load its saved session if present

Runner:

- `<leader>ri` show runner info for the current buffer
- `<leader>rb` build the current project or buffer target
- `<leader>rr` run the current project or buffer target
- `<leader>rt` run the current project or package tests
- `<leader>rT` run tests for the current file when supported
- `<leader>rl` repeat the last runner command
- `<leader>rc` clean the current project or buffer target

Workflow and tools:

- `<leader>lt` open a shell in the current working directory
- `<leader>lp` open a shell in the current project root
- `<leader>lb` open a shell in the current buffer directory
- `<leader>lT` reopen the last floating terminal command
- `<leader>gj` Jujutsu TUI
- `<leader>js` Jujutsu status
- `<leader>jl` Jujutsu log
- `<leader>jd` Jujutsu diff
- `<leader>ld` LazyDocker
- `<leader>ls` Live Server
- `<leader>db` toggle DBUI
- `<leader>df` find database buffer
- `<leader>dr` rename database buffer
- `<leader>dq` last query info

Git hunks:

- `[h` / `]h` previous or next hunk
- `<leader>ghs` stage hunk
- `<leader>ghr` reset hunk
- `<leader>ghp` preview hunk
- `<leader>ghb` blame line
- `<leader>ghq` send current file hunks to quickfix
- `<leader>ghl` send current file hunks to location list
- `<leader>ghS` stage current buffer
- `<leader>ghu` undo last staged hunk
- `<leader>ghD` diff current buffer

Debug:

- `<leader>Dc` continue / launch
- `<leader>Db` toggle breakpoint
- `<leader>DB` conditional breakpoint
- `<leader>Do` step over
- `<leader>Di` step into
- `<leader>DO` step out
- `<leader>Dr` debug REPL
- `<leader>Dl` run last
- `<leader>Dt` terminate
- `<leader>DC` run to cursor
- `<leader>Du` toggle DAP UI

Runner:

- `<localleader>rb` build
- `<localleader>rr` run
- `<localleader>rt` test
- `<localleader>rT` test current file for Python and supported frontend runners
- `<localleader>rk` show runner keymaps
- `<localleader>rl` repeat last runner command
- `<localleader>rc` clean

Plugin maintenance:

- `<leader>pu` plugin update picker
- `<leader>pP` update all plugins
- `<leader>ps` plugin status picker
- `<leader>pr` plugin repair picker
- `<leader>pl` pack log
- `<leader>pR` pack repair pending
- `<leader>pC` pack repair clean
- `<leader>pU` pack sync lockfile

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
- `<leader>xx` diagnostics list
- `<leader>xX` buffer diagnostics list
- `<leader>xq` diagnostics to quickfix
- `<leader>xl` diagnostics to location list
- `<leader>xc` clear quickfix list
- `<leader>xC` clear location list
- `<leader>cl` run codelens
- `<leader>cs` document symbols
- `<leader>xQ` quickfix list
- `<leader>xL` location list
- `w/e/b/ge` smart subword motions via `nvim-spider`

Filetype-local actions use `<localleader>`.

Examples:

- Markdown: `<localleader>m*`
- `package.json`: `<localleader>n*`
- Hurl: `<localleader>h*`
- Frontend buffers: `<localleader>l*`, `<localleader>t*`, `<localleader>v*`
- Runner-enabled buffers: `<localleader>r*`

C / C++ (`<localleader>c*`):

- `<localleader>ck` show C/C++ keymaps
- `<localleader>cb` build file or CMake target
- `<localleader>cr` run file or CMake target
- `<localleader>cc` clean file or CMake target
- `<localleader>ct` run tests
- `<localleader>ch` switch source/header
- `<localleader>ca` show AST
- `<localleader>ci` show symbol info
- `<localleader>cT` show type hierarchy
- `<localleader>cm` show memory usage
- `<localleader>cg` generate CMake project
- `<localleader>cs` select build target
- `<localleader>cl` select launch target
- `<localleader>cv` select build type
- `<localleader>cp` select configure preset

Go (`<localleader>`):

- `<localleader>fs` fill struct
- `<localleader>ie` add if err
- `<localleader>at` add tags
- `<localleader>aT` remove tags
- `<localleader>im` generate implementation
- `<localleader>tf` test function
- `<localleader>ta` test package
- `<localleader>tc` test coverage

## Structure

```text
nvim/
├── init.lua
├── nvim-pack-lock.json
├── after/
│   └── ftplugin/
├── docs/
├── lua/
│   ├── core/      # options, diagnostics, keymaps, commands, terminal helpers, shared utilities
│   ├── lang/      # runtime language modes and shared helpers like frontend state/actions
│   ├── lsp/       # LSP registry and setup
│   ├── pack/      # vim.pack specs, commands, and UI
│   ├── plugins/   # plugin setup grouped by responsibility such as editing/completion/git/format
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

- [lua/user/init.lua](/d:/config/dotfiles/nvim/lua/user/init.lua) top-level user override entrypoint that merges the optional modules below
- [lua/user/theme.lua](/d:/config/dotfiles/nvim/lua/user/theme.lua) theme preference and highlight overrides
- [lua/user/pack.lua](/d:/config/dotfiles/nvim/lua/user/pack.lua) disable base plugins or add small `vim.pack` specs
- [lua/user/session.lua](/d:/config/dotfiles/nvim/lua/user/session.lua) session behavior toggles
- [lua/user/lsp_settings.lua](/d:/config/dotfiles/nvim/lua/user/lsp_settings.lua) Mason-backed LSP install toggles
- [lua/user/options.lua](/d:/config/dotfiles/nvim/lua/user/options.lua) option overrides
- [lua/user/keymaps.lua](/d:/config/dotfiles/nvim/lua/user/keymaps.lua) extra keymaps
- [lua/user/autocmds.lua](/d:/config/dotfiles/nvim/lua/user/autocmds.lua) extra autocmds
- [lua/user/commands.lua](/d:/config/dotfiles/nvim/lua/user/commands.lua) custom commands
- [lua/user/lang.lua](/d:/config/dotfiles/nvim/lua/user/lang.lua) treesitter, formatters, frontend mode defaults, extra Mason tools
- [lua/user/plugins/](/d:/config/dotfiles/nvim/lua/user/plugins) extra plugins
- [lua/user/lsp/](/d:/config/dotfiles/nvim/lua/user/lsp) extra or overridden LSP servers
- [after/ftplugin/](/d:/config/dotfiles/nvim/after/ftplugin) per-filetype local behavior

See [docs/customization.md](/d:/config/dotfiles/nvim/docs/customization.md) for examples.
See [docs/architecture.md](/d:/config/dotfiles/nvim/docs/architecture.md) for ownership boundaries before refactoring base modules.

## Installation

Requirements:

- Neovim 0.12 or newer
- `git`
- Nerd Font recommended
- External tools as needed for your languages, for example `uv`, `ty`, `ruff`, `stylua`, `goimports`, `gofumpt`, `shfmt`, `prettier`, `taplo`, `clang-format`, `debugpy`, `codelldb`

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
- `:Health`
- `:Health vim.lsp`
- `:messages`

Windows note:

- If plain `:checkhealth` used to freeze, this config now ships a local Mason health shim to avoid an upstream Windows log-permission hang in `:checkhealth mason`

## Notes From `cap153-nvim`

Most framework-level ideas from `cap153-nvim` were intentionally not carried forward. The current config already absorbed the parts that aged well:

- `snacks.nvim` as the main dashboard and picker layer
- `blink.cmp`
- `noice.nvim`
- direct Neovim 0.11+ / 0.12+ LSP style instead of framework glue
- the older custom dashboard header

Still potentially useful as future borrow candidates:

- optional GUI-specific helpers for Neovide

Likely next improvements:

- move heavy optional tools toward command or filetype-triggered loading once `vim.pack` lazy patterns settle
- add a richer test UI only if the extra adapter surface is worth the maintenance cost for your main languages

Already borrowed into the new base:

- markdown table auto-formatting with a manual `:MarkdownTableFormat` command
- a few picker interaction details such as `<Esc>`, `<C-e>`, and `<C-u>`
- `snacks.nvim` lazygit integration

Those are treated as optional enhancements, not core architecture.
