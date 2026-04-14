# mcge's Vim Configuration

> Modern, modular, and explicit

[中文文档](./README_CN.md)

A production-ready Vim configuration written in **Vim9script** with a small, explicit architecture, intelligent lazy loading, and comprehensive LSP support via CoC.nvim.

---

## ✨ Features

### 🏗️ Hybrid Architecture

```
Bootstrap → Core → Modules → Config → Local
     ↓         ↓        ↓         ↓        ↓
  Environ   Utils   Features  Simple   User
```

- **Bootstrap** - Environment initialization, constants, basic settings
- **Core** - Error handling, utilities, module loader, health check
- **Modules** - Feature modules with real setup logic
- **Config** - Shared mappings and static plugin-native configuration
- **Local** - User-specific overrides and machine-local values

Standard filetype behavior now lives in `after/ftplugin`, so language-specific settings are loaded by Vim's native runtime mechanism instead of a custom loader.

Directory ownership rules live in [ARCHITECTURE.md](./ARCHITECTURE.md).

### ⚡ Performance Optimization

- **Smart Lazy Loading** - Modules load only when helpful
- **Performance Monitoring** - Built-in startup time tracking & module stats
- **Health Checks** - Auto-detect config, dependencies & plugin status
- **Deferred Init** - Non-critical modules load later

### 🌐 CoC-Based Language Support

- **System**: C/C++ (clangd), Java, CMake, Zig
- **Web**: TypeScript/JavaScript, HTML, CSS/SCSS, Vue 2/3, React, TailwindCSS v3
- **Dynamic**: Python (Pyright), Lua, Clojure, Shell
- **Systems**: Rust (rust-analyzer)
- **Data**: JSON, YAML, TOML, XML, SQL
- **Tools**: Git, Prettier, ESLint, AI Completion (TabNine)

### 🔧 Core Plugins

**LSP & Completion**
- CoC.nvim - Full LSP support with project-specific extensions

**Search & Navigation**
- **Clap** - Modern fuzzy finder (faster than FZF)
- **Vista** - Code outline and symbol navigation
- **CoC Explorer** - File browser

**UI & Appearance**
- **Startify** - Start screen
- **Airline** - Status line
- **Which-key** - Key binding hints

**Editing Enhancements**
- **Floaterm** - Floating terminal
- **vim-visual-multi** - Multi-cursor editing
- **NERDCommenter** - Comments
- **vim-matchup** - Enhanced matching

---

## 📦 Installation

### Requirements

**Required**
- Vim 9.0+ (`vim9script`; use the separate `nvim/` config for Neovim)
- Node.js 16+ (for CoC.nvim)
- Git

**Recommended**
- `ripgrep` (rg) - Fast text search
- `fd` - Fast file finding
- `ctags` - Code tags (for Vista)

### Windows Installation

```powershell
# 1. Install recommended tools
winget install BurntSushi.ripgrep.MSVC
winget install sharkdp.fd
winget install UniversalCtags.UniversalCtags

# 2. Clone configuration
git clone <your-repo> vimrc

# 3. Run install script
cd vimrc
.\install.bat
```

### Linux/macOS Installation

```bash
# 1. Install recommended tools
# Ubuntu/Debian
sudo apt install ripgrep fd-find universal-ctags

# macOS
brew install ripgrep fd ctags

# 2. Clone configuration
git clone <your-repo> vimrc

# 3. Run install script
cd vimrc
chmod +x install.sh
./install.sh
```

### First Launch

1. Start Vim: `vim`
2. CoC extensions will auto-install (takes a few minutes on first launch)
3. Run health check: `:VimrcLoadReport`
4. View startup time: `:VimStartupTime`

---

## 🗂️ Directory Structure

### Hybrid Architecture

```
vimrc/
├── init.vim                    # Main entry point
├── ARCHITECTURE.md             # Directory ownership rules
├── bootstrap/                  # Environment & basic settings
│   ├── constants.vim           # Global constants
│   ├── environment.vim         # Environment detection
│   └── settings.vim            # Basic Vim settings
├── core/                       # Core functionality
│   ├── error_handler.vim       # Error handling
│   ├── utils.vim               # Utility functions
│   ├── loader.vim              # Module loader
│   ├── keymap.vim              # Declarative keymap helpers
│   ├── module.vim              # Shared module helpers
│   └── health.vim              # Health check system
├── modules/                    # Feature modules
│   ├── editor/                 # Editor enhancements
│   │   ├── commenter.vim       # NERDCommenter
│   │   ├── match-pair.vim      # vim-matchup
│   │   ├── multi-cursor.vim    # vim-visual-multi
│   │   ├── snippets.vim        # UltiSnips
│   │   ├── tabsize.vim         # Tab configuration
│   │   ├── tags.vim            # gutentags
│   │   └── whitespace.vim      # Whitespace highlighting
│   ├── git/                    # Git integration
│   │   └── gutter.vim          # vim-gitgutter
│   ├── lsp/                    # LSP configuration
│   │   └── coc.vim             # CoC setup
│   ├── navigation/             # Navigation
│   │   ├── clap.vim            # Clap fuzzy finder
│   │   └── vista.vim           # Vista outline
│   ├── terminal/               # Terminal
│   │   └── floaterm.vim        # Floaterm
│   └── ui/                     # UI configuration
│       ├── airline.vim         # Airline statusline
│       ├── appearance.vim      # GUI settings
│       ├── colorscheme.vim     # Color scheme
│       ├── startify.vim        # Start screen
│       └── whichkey.vim        # Which-key integration
├── config/                     # Shared configuration
│   ├── mapping/                # Centralized keymap ownership
│   │   ├── basic.vim           # Core global mappings
│   │   ├── editor.vim          # Editor plugin mappings
│   │   ├── git.vim             # Git mappings
│   │   ├── lsp.vim             # CoC/LSP mappings
│   │   ├── navigation.vim      # Clap/Vista mappings
│   │   ├── terminal.vim        # Floaterm mappings
│   │   └── ui.vim              # Which-key trigger mappings
│   └── coc-settings.json       # CoC JSON config
├── after/
│   └── ftplugin/               # Standard filetype-local overrides
│       ├── ftplugin.template.vim
│       ├── python.vim
│       ├── rust.vim
│       ├── typescript.vim
│       ├── typescriptreact.vim
│       ├── vim.vim
│       └── zig.vim
├── local/                      # User customizations
│   ├── user_env.vim            # User environment variables
│   ├── module_overrides.example.vim
│   ├── user_mappings.example.vim
│   ├── user_settings.example.vim
│   ├── user_settings.vim       # User settings
│   ├── module_overrides.vim    # Optional module config overrides
│   └── user_mappings.vim       # Optional personal keybindings
└── pack/mcge/start/            # Vim plugins
```

**Design Principles:**
- **modules/** - Complex features with plugin setup, helper functions, and health checks
- **config/** - Centralized shared mappings and static cross-feature configuration
- **after/ftplugin/** - Filetype-local behavior using Vim's standard runtime mechanism
- **local/** - Personal overrides without changing shared module ownership

Start from `modules/module.template.vim`, `after/ftplugin/ftplugin.template.vim`,
`local/module_overrides.example.vim`, `local/user_settings.example.vim`, and
`local/user_mappings.example.vim` when you add new shared modules, filetype
overrides, or local customizations.

**Decision Guide**
- Put plugin setup and feature logic in `modules/`.
- Put shared global mappings in `config/mapping/` and register them through `core/keymap.vim`.
- Put filetype-local settings in `after/ftplugin/`.
- Keep `config/` small and declarative.
- Keep module conventions, file header rules, and helper usage aligned with [ARCHITECTURE.md](./ARCHITECTURE.md).

---

## ⌨️ Key Bindings

### Leader Key: `<Space>`

The complete shortcut list is generated from the runtime keymap registry:

- [docs/keymaps.md](./docs/keymaps.md)

That file is the single source of truth for shared mappings. After changing
`config/mapping/*.vim` or `core/keymap.vim`, regenerate and verify it with:

```powershell
pwsh -File vimrc/scripts/verify_keymaps.ps1
```

When adding a new `config/mapping/*.vim` file, keep the file shape consistent:
put shared `const ..._defaults` tables at the top, then which-key groups, then
spec lists, and finally one `g:MapMany()` / `g:CmdMapMany()` /
`g:PlugMapMany()` call per list. If a file still needs one-off mappings, use
`g:MapSpec(spec, defaults)` so repeated options do not spread back into every
entry.

Representative namespaces:

- Search and navigation: `<leader>p`, `<leader>/`, `<leader>sg`, `<leader>sw`
- LSP and actions: `gd`, `gr`, `<leader>rn`, `<leader>af`, `<leader>aa`
- Explorer and outline: `<leader>ee`, `<leader>vv`, `<leader>vf`
- Windows and buffers: `<C-h/j/l>`, `<C-tab>`, `<C-x><C-s>`, `<leader>rr`
- Terminal: `<F7>`, `<F8>`, `<F9>`, `<F12>`

---

## ⚙️ Configuration

### User Environment Variables

Edit `local/user_env.vim`:

```vim
vim9script

# Author info
g:mcge_custom_author = "Your Name"
g:mcge_custom_email = "<your@email.com>"

# Vista backend (coc or ctags)
g:mcge_custom_vista_executive = "coc"

# Windows paths
if has('win32') || has('win64')
  g:mcge_custom_project = "E:/MyProjects"
  g:mcge_custom_workspace = "E:/Workspaces"
endif
```

### User Settings

Edit `local/user_settings.vim`:

```vim
vim9script

def ApplyUserEditorOverrides()
  set number
  set relativenumber
enddef

def ApplyUserUiOverrides()
  set cursorline
enddef

def RegisterUserAutocmds()
  augroup mcge_local_user_custom
    autocmd!
    autocmd BufEnter *.md setlocal wrap
  augroup END
enddef
```

Put most personal overrides in `local/user_settings.vim`.
Prefer keeping it split into `ApplyUserEditorOverrides()`,
`ApplyUserUiOverrides()`, `RegisterUserCommands()`, and
`RegisterUserAutocmds()`.
If you want to keep personal mappings isolated, you can optionally create
`local/user_mappings.vim`, but it is not required by the shared architecture.

### Module Overrides

Use `local/module_overrides.vim` when you want to change a shared module's
`config` fields without editing `modules/*`.

```vim
vim9script

g:SetModuleOverride('colorscheme', {scheme: 'desert'})
g:SetModuleOverride('clap', {layout: {width: '90%'}})
```

Copy `local/module_overrides.example.vim` as a starting point. This is the
preferred user entrypoint for shared module config changes.

### Filetype Overrides

Put language-local settings in `after/ftplugin/<filetype>.vim`.
This keeps startup config smaller and lets Vim load filetype behavior only when the matching buffer is opened.

### CoC Configuration

Edit `config/coc-settings.json`:

```json
{
  "python.linting.enabled": true,
  "python.formatting.provider": "black",
  "rust-analyzer.checkOnSave.command": "clippy"
}
```

---

## 🔧 Operations

### Shared Reports

After changing `config/mapping/*.vim`, `core/keymap.vim`, or which-key registry
behavior, run:

```powershell
pwsh -File vimrc/scripts/verify_keymaps.ps1
```

This command regenerates `docs/keymaps.md`, checks for UTF-8/LF output without
embedded `NUL` bytes, and runs `git diff --check` on the keymap-related files.

For runtime inspection, use the shared entry points first:

```vim
:CheckHealth             " Shared health report
:VimrcLoadReport         " Module loading report
:VimStartupTime          " Startup timing
```

`CheckHealth` now shows module override/config metadata more directly, including
`module_id`, whether local overrides are active, override keys, and config key
summaries for managed modules.

### Plugin-Specific Status

Use plugin-native commands when you are debugging a specific feature. Common
examples are `:CocInfo`, `:Vista info`, and `:Clap files`, but the exact set
depends on which modules are enabled.

---

## 🐛 Troubleshooting

1. Start with `:CheckHealth` and `:VimrcLoadReport` to separate dependency
   failures from module-load failures.
2. For keymap/doc drift, rerun `pwsh -File vimrc/scripts/verify_keymaps.ps1`.
3. For CoC/Vista/Clap issues, verify external tools such as `node`, `rg`,
   `fd`, and `ctags`, then inspect plugin-native status commands like
   `:CocInfo` or `:Vista info`.

---

## 📚 Extending

### Add Plugins

```bash
cd pack/mcge/start
git clone https://github.com/author/plugin-name
```

After adding a plugin, start from `modules/module.template.vim`, then wire its
behavior into `modules/`, `config/`, or
`after/ftplugin/` according to [ARCHITECTURE.md](./ARCHITECTURE.md).

### Add CoC Extensions

```vim
:CocInstall coc-extension-name
```

### Inspect Module State

Use the shared health report and selected module helpers to inspect runtime
state, for example:

```vim
:CheckHealth
:call g:ClapHealthCheck()
:call g:VistaHealthCheck()
:call g:AirlineHealthCheck()
```

---

## 🔗 Resources

- [Vim 9 Documentation](https://vimhelp.org/vim9.txt.html)
- [CoC.nvim](https://github.com/neoclide/coc.nvim)
- [vim-clap](https://github.com/liuchengxu/vim-clap)
- [Vista.vim](https://github.com/liuchengxu/vista.vim)
- [vim-airline](https://github.com/vim-airline/vim-airline)
- [vim-startify](https://github.com/mhinz/vim-startify)

---

## 📄 License

MIT License

## 🙏 Acknowledgments

- CoC.nvim team
- All plugin authors
- Vim community

---

**Enjoy your Vim journey!** 🎉
