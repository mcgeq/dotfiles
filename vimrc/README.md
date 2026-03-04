# mcge's Vim Configuration

> **Version 2.0** - Modern, Modular, High-Performance

[中文文档](./README_CN.md)

A production-ready Vim configuration written in **Vim9script** with hybrid modular architecture, intelligent lazy loading, and comprehensive LSP support via CoC.nvim.

---

## ✨ Features

### 🏗️ Hybrid Architecture (v2.0)

```
Bootstrap → Core → Modules → Config → Local
     ↓         ↓        ↓         ↓        ↓
  Environ   Utils   Features  Simple   User
```

- **Bootstrap** - Environment initialization, constants, basic settings
- **Core** - Error handling, utilities, module loader, health check
- **Modules** - Feature modules with config dicts, health checks (20 files)
- **Config** - Simple configurations, language-specific settings (8 files)
- **Local** - User customizations (not tracked by Git)

### ⚡ Performance Optimization

- **Smart Lazy Loading** - Modules load on demand, startup < 100ms
- **Performance Monitoring** - Built-in startup time tracking & module stats
- **Health Checks** - Auto-detect config, dependencies & plugin status
- **Deferred Init** - Non-critical modules load later

### 🌐 Language Support (45+ CoC Extensions)

- **System**: C/C++ (clangd), Java, CMake, Zig
- **Web**: TypeScript/JavaScript, HTML, CSS/SCSS, Vue 2/3, React, TailwindCSS v3
- **Dynamic**: Python (Pyright), Lua, Clojure, Shell
- **Systems**: Rust (rust-analyzer)
- **Data**: JSON, YAML, TOML, XML, SQL
- **Tools**: Git, Prettier, ESLint, AI Completion (TabNine)

### 🔧 Core Plugins

**LSP & Completion**
- CoC.nvim - Full LSP support with 45+ extensions

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
- Vim 9.0+ or Neovim 0.8+
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
├── bootstrap/                  # Environment & basic settings
│   ├── constants.vim           # Global constants
│   ├── environment.vim         # Environment detection
│   └── settings.vim            # Basic Vim settings
├── core/                       # Core functionality
│   ├── error_handler.vim       # Error handling
│   ├── utils.vim               # Utility functions
│   ├── loader.vim              # Module loader
│   └── health.vim              # Health check system
├── modules/                    # Feature modules (20 files)
│   ├── editor/ (7)             # Editor enhancements
│   │   ├── commenter.vim       # NERDCommenter
│   │   ├── match-pair.vim      # vim-matchup
│   │   ├── multi-cursor.vim    # vim-visual-multi
│   │   ├── snippets.vim        # UltiSnips
│   │   ├── tabsize.vim         # Tab configuration
│   │   ├── tags.vim            # gutentags
│   │   └── whitespace.vim      # Whitespace highlighting
│   ├── git/ (2)                # Git integration
│   │   ├── gutter.vim          # vim-gitgutter
│   │   └── mapping.vim         # Git keybindings
│   ├── lsp/ (2)                # LSP configuration
│   │   ├── coc.vim             # CoC setup
│   │   └── mapping.vim         # LSP keybindings
│   ├── navigation/ (3)         # Navigation
│   │   ├── clap.vim            # Clap fuzzy finder
│   │   ├── mapping.vim         # Navigation keys
│   │   └── vista.vim           # Vista outline
│   ├── terminal/ (1)           # Terminal
│   │   └── floaterm.vim        # Floaterm
│   └── ui/ (5)                 # UI configuration
│       ├── airline.vim         # Airline statusline
│       ├── appearance.vim      # GUI settings
│       ├── colorscheme.vim     # Color scheme
│       ├── startify.vim        # Start screen
│       └── statusline.vim      # Statusline config
├── config/                     # Simple configurations (8 files)
│   ├── lang/ (6)               # Language-specific settings
│   │   ├── python.vim
│   │   ├── rust.vim
│   │   ├── typescript.vim
│   │   ├── zig.vim
│   │   ├── css.vim
│   │   └── html.vim
│   ├── mapping/
│   │   └── basic.vim           # Basic global mappings
│   ├── ftplugin/
│   │   └── vim.vim             # Vim ftplugin
│   └── coc-settings.json       # CoC JSON config
├── local/                      # User customizations
│   ├── user_env.vim            # User environment variables
│   ├── user_settings.vim       # User settings
│   └── user_mappings.vim       # User keybindings
└── pack/mcge/start/            # Vim plugins
```

**Design Principles:**
- **modules/** - Complex features with config dicts, functions, health checks
- **config/** - Simple settings, language-specific autocmds, basic mappings

---

## ⌨️ Key Bindings

### Leader Key: `<Space>`

### Startify Start Screen

| Key | Function |
|-----|----------|
| `n` | New file |
| `f` | File search (Clap) |
| `o` | Recent files |
| `w` | Text search |
| `s` | Load session |
| `c` | Open config |

### Clap Search

| Shortcut | Function |
|----------|----------|
| `<leader>p` | File search |
| `<leader>P` | Git files |
| `<leader>/` | Text search |
| `<leader>fg` | Grep search |
| `<leader>bb` | Buffers |
| `<leader>fh` | Recent files |
| `<leader>fl` | Current file lines |
| `<leader>gc` | Git commits |
| `<leader>:` | Command search |
| `<leader>;` | Command history |
| `<leader>km` | Key mappings |
| `<leader>?` | Help tags |
| `<leader>tc` | Colorschemes |

### Vista Code Outline

| Shortcut | Function |
|----------|----------|
| `<F8>` | Toggle outline |
| `<leader>v` | Toggle outline |
| `<leader>vf` | Symbol search |
| `<leader>vc` | Use CoC backend |
| `<leader>vt` | Use ctags backend |

### CoC LSP

**Code Navigation**
- `gd` - Go to definition
- `gy` - Go to type definition
- `gi` - Go to implementation
- `gr` - Find references
- `K` - Show documentation

**Code Actions**
- `<leader>rn` - Rename symbol
- `<leader>f` - Format code
- `<leader>a` - Code actions
- `<leader>ac` - Code action (cursor)
- `[g` / `]g` - Previous/next diagnostic

**CoC Explorer**
- `<leader>e` - Open explorer
- `<leader>ed` - Explorer (directory)
- `<leader>ef` - Explorer (floating)
- `<leader>y` - Yank history

### Windows and Buffers

- `<Ctrl-h/j/k/l>` - Switch windows
- `<Ctrl-tab>` / `<Ctrl-s-tab>` - Next/previous buffer
- `<Ctrl-x><Ctrl-s>` - Save file
- `<Ctrl-x><Ctrl-q>` - Save and quit

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

# Your custom settings
set number
set relativenumber
```

### User Key Bindings

Edit `local/user_mappings.vim`:

```vim
vim9script

# Your custom keybindings
nnoremap <leader>w :w<CR>
nnoremap <leader>q :q<CR>
```

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

## 🔧 Common Commands

### Performance and Debugging

```vim
:VimStartupTime          " View startup time
:VimrcLoadReport         " Module loading report
:CheckHealth             " Health check
```

### Clap Search

```vim
:Clap files              " File search
:Clap grep               " Text search
:Clap buffers            " Buffers
:Clap history            " Recent files
:Clap command            " Commands
:Clap command_history    " Command history
:Clap maps               " Key mappings
:Clap help_tags          " Help tags
:Clap colors             " Colorschemes
```

### Vista Outline

```vim
:Vista                   " Toggle outline
:Vista finder            " Symbol search
:Vista coc               " Use CoC backend
:Vista ctags             " Use ctags backend
:Vista info              " Show info
```

### CoC

```vim
:CocInfo                 " CoC info
:CocList extensions      " Extension list
:CocCommand explorer     " File browser
:Format                  " Format code
:OR                      " Organize imports
:CocRestart              " Restart CoC
```

---

## 🐛 Troubleshooting

### CoC Not Working

1. Check Node.js: `node --version` (requires 16+)
2. View status: `:CocInfo`
3. Restart CoC: `:CocRestart`
4. Check extensions: `:CocList extensions`

### Search is Slow

1. Ensure ripgrep is installed: `rg --version`
2. Use Clap: `:Clap files`
3. Use CoC List: `:CocList files`

### Vista Not Showing Symbols

1. Check file type: `:Vista info`
2. Switch backend: `:Vista coc`
3. Check CoC: `:CocInfo`

### Module Loading Issues

```vim
:VimrcLoadReport         " Check which modules failed
:echo g:mcge_startup_time " View startup time in ms
```

---

## 🚀 Performance Metrics

- **Startup Time**: ~80-100ms
- **Modules**: 20 feature modules
- **CoC Extensions**: 45+

### View Performance

```vim
:VimStartupTime          " Startup time
:VimrcLoadReport         " Module loading report
:CheckHealth             " Health status
```

---

## 📚 Advanced Usage

### Adding Plugins

```bash
cd pack/mcge/start
git clone https://github.com/author/plugin-name
```

### Adding CoC Extensions

```vim
:CocInstall coc-extension-name
:CocUninstall coc-extension-name
```

### Module Health Checks

Each module provides a health check function:

```vim
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
