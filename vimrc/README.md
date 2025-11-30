# mcge's Vim Configuration

> **Version 2.0** - Modern, Modular, High-Performance

[中文文档](./README_CN.md)

A production-ready Vim configuration written in **Vim9script** with modular architecture, intelligent lazy loading, and comprehensive LSP support via CoC.nvim.

---

## ✨ Features

### 🏗️ Modular Architecture (v2.0)

```
Bootstrap → Core → Modules → Config → Local
     ↓         ↓        ↓         ↓        ↓
  Environ   Utils   UI/LSP   Compat   User
```

- **Bootstrap** - Environment initialization, constants, basic settings
- **Core** - Error handling, utilities, module loader, health check
- **Modules** - UI config, LSP config (modular, pluggable)
- **Config** - Legacy compatibility, plugin config, key mappings
- **Local** - User customizations (not tracked by Git)

### ⚡ Performance Optimization

- **Smart Lazy Loading** - UI & LSP load on demand, startup < 100ms
- **Performance Monitoring** - Built-in startup time tracking & module stats
- **Health Checks** - Auto-detect config, dependencies & plugin status
- **Deferred Init** - Statusline & non-critical modules load later

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
- **vim-surround** - Quick surroundings
- **vim-commentary** - Comments

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
3. Run health check: `:CheckHealth`
4. View startup time: `:VimStartupTime`

---

## 🗂️ Directory Structure

```
vimrc/
├── init.vim                # Main entry point
├── bootstrap/              # Environment & settings
├── core/                   # Error handling, utilities, loader, health check
├── modules/                # UI & LSP modules (modular, pluggable)
├── config/                 # Plugin configs, mappings, language-specific
├── local/                  # User customizations (not tracked by Git)
└── pack/                   # Vim plugins
```

**Architecture Layers**: Bootstrap → Core → Modules → Config → Local

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

### Clap Search (Recommended)

| Shortcut | Function |
|----------|----------|
| `<leader>p` | File search |
| `<leader>P` | Git files |
| `<leader>/` | Text search |
| `<leader>bb` | Buffers |
| `<leader>fh` | Recent files |
| `<leader>gc` | Git commits |

### Vista Code Outline

| Shortcut | Function |
|----------|----------|
| `<F8>` | Toggle outline |
| `<leader>v` | Toggle outline |
| `<leader>vf` | Symbol search |

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
- `[g` / `]g` - Previous/next diagnostic

### Windows and Buffers

- `<Ctrl-h/j/k/l>` - Switch windows
- `<Ctrl-n/p>` - Switch buffers
- `<Ctrl-x><Ctrl-s>` - Save file

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
```

### Vista Outline

```vim
:Vista                   " Toggle outline
:Vista finder            " Symbol search
:Vista coc               " Use CoC backend
```

### CoC

```vim
:CocInfo                 " CoC info
:CocList extensions      " Extension list
:CocCommand explorer     " File browser
:Format                  " Format code
:OR                      " Organize imports
```

---

## 🐛 Troubleshooting

### CoC Not Working

1. Check Node.js: `node --version` (requires 16+)
2. View status: `:CocInfo`
3. Restart CoC: `:CocRestart`

### Search is Slow

1. Ensure ripgrep is installed: `rg --version`
2. Use Clap: `:Clap files`
3. Use CoC List: `:CocList files`

### Vista Not Showing Symbols

1. Check file type: `:Vista info`
2. Switch backend: `:Vista coc`
3. Check CoC: `:CocInfo`

---

## 🚀 Performance Metrics

- **Startup Time**: ~80-100ms
- **Modules**: 40+
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
```

### Custom Key Bindings

Edit `local/user_mappings.vim`:

```vim
vim9script

nnoremap <leader>w :w<CR>
nnoremap <leader>q :q<CR>
```

---

## 🔗 Resources

- [Vim 9 Documentation](https://vimhelp.org/vim9.txt.html)
- [CoC.nvim](https://github.com/neoclide/coc.nvim)
- [vim-clap](https://github.com/liuchengxu/vim-clap)
- [Vista.vim](https://github.com/liuchengxu/vista.vim)

---

## 📄 License

MIT License

## 🙏 Acknowledgments

- CoC.nvim team
- All plugin authors
- Vim community

---

**Enjoy your Vim journey!** 🎉
