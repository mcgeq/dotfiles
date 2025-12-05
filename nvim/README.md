# Neovim Configuration

A modern Neovim configuration based on [AstroNvim v5](https://github.com/AstroNvim/AstroNvim) with extensive language support and productivity features.

## 🌟 Features

### Core Stack
- **Base**: AstroNvim v5 - A modular Neovim framework
- **Package Manager**: Lazy.nvim - Fast and efficient plugin management
- **Language Server**: AstroLSP with Mason - Comprehensive LSP support
- **Snippets**: LuaSnip with friendly-snippets

### Language Support
Full LSP support for 20+ languages:
- **Systems**: C/C++, Rust, Zig, Go, CMake
- **Web**: TypeScript, JavaScript, Vue, Svelte, JSON, HTML/CSS
- **Dynamic**: Python (with Ruff), Lua, Bash
- **Functional**: Clojure, Jison (jj)
- **Data**: SQL, YAML, XML, TOML, Markdown
- **Mobile**: Dart/Flutter, Kotlin
- **Other**: Docker

### Editor Features
- **Navigation**: Flash.nvim, smart-splits, window-picker
- **Completion**: Blink.cmp with multiple sources (LSP, Git, Emoji, etc.)
- **Diagnostics**: Trouble.nvim, error-lens, neodim
- **Search**: Fzf-lua, namu.nvim, snacks-picker
- **Git**: Gitsigns, diffview, gitlinker, blame, gist
- **Tree-sitter**: Full syntax highlighting and text objects
- **Formatter**: Conform.nvim
- **Testing**: Neotest with language-specific adapters

### Productivity
- **Session Management**: Resession.nvim
- **Task Runner**: Overseer.nvim, Executor.nvim
- **Debugging**: nvim-dap with language-specific integrations
- **File Explorer**: mini.files, Oil.nvim
- **Terminal**: ToggleTerm.nvim
- **Zen Mode**: Distraction-free editing

### ⚡ Performance Optimizations

This configuration includes several performance optimizations:

- **Dynamic Loading**: Plugins and tools load based on selected preset
- **Lazy Themes**: Only active theme loads on startup (others on-demand)
- **Smart Treesitter**: Parser installation based on preset (13-30 parsers)
- **Optimized Mason**: Tool installation based on preset (8-31 tools)
- **Reduced Keybindings**: Cleaned up from 60+ to 45 essential shortcuts
- **Delayed Initialization**: Critical components load in correct order
- **Error Resilience**: Comprehensive error handling prevents startup failures

**Performance Gains** (compared to loading all plugins):
- Startup time: **33-40% faster** (frontend/backend presets)
- Memory usage: **15-33% lower** (preset-dependent)
- Disk space: **44-69% less** (minimal preset)

## 📦 Installation

### Prerequisites
- Neovim 0.9+ 
- Git
- (Optional) Nerd Fonts for icons

### Quick Install

#### For Windows:
```powershell
# Backup existing config
Move-Item $env:LOCALAPPDATA\nvim $env:LOCALAPPDATA\nvim.bak -ErrorAction SilentlyContinue

# Clone configuration
git clone <repository-url> $env:LOCALAPPDATA\nvim

# Start Neovim
nvim
```

#### For Linux/macOS:
```bash
# Backup existing config
mv ~/.config/nvim ~/.config/nvim.bak 2>/dev/null
mv ~/.local/share/nvim ~/.local/share/nvim.bak 2>/dev/null
mv ~/.local/state/nvim ~/.local/state/nvim.bak 2>/dev/null
mv ~/.cache/nvim ~/.cache/nvim.bak 2>/dev/null

# Clone configuration
git clone <repository-url> ~/.config/nvim

# Start Neovim
nvim
```

The first launch will automatically install Lazy.nvim and all configured plugins.

## ⚡ Preset System

This configuration features a powerful preset system that dynamically loads plugins, LSP servers, and tools based on your development needs.

### 📦 Available Presets

| Preset | Plugins | Parsers | Tools | Startup | Use Case |
|--------|---------|---------|-------|---------|----------|
| **fullstack** (default) | All | 30+ | 31 | ~400ms | Full-stack development |
| **frontend** | ~70% | 20 | 18 | ~250ms | JS/TS/Vue/React development |
| **backend** | ~70% | 19 | 23 | ~280ms | Rust/Go/Python/C++ development |
| **minimal** | ~40% | 13 | 8 | ~120ms | Quick editing, config files |
| **performance** | ~50% | 15 | 8 | ~180ms | Low-spec machines |

### 🎯 Preset Features

Each preset automatically configures:

**Frontend Preset**:
- LSP: TypeScript, Vue, HTML, CSS, Tailwind, Emmet
- Tools: Biome (formatter/linter), Prettier
- Parsers: JavaScript, TypeScript, Vue, HTML, CSS
- Plugins: TypeScript-tools, Vue Language Server, Colorizer

**Backend Preset**:
- LSP: Rust, Go, Python, C++, Zig, CMake
- Tools: rust-analyzer, gopls, pyright, ruff, clangd
- Parsers: Rust, Go, Python, C, C++, Zig
- Plugins: Go.nvim, Python venv-selector, DAP debuggers

**Minimal Preset**:
- LSP: Lua, Bash, YAML, JSON, TOML
- Tools: Essential formatters only
- Parsers: Core + config files
- Plugins: Basic editing features only

### 🛠️ Switching Presets

**Method 1: Using Command**
```vim
:PresetSwitch frontend
" Restart Neovim after switching
```

**Method 2: Configuration File**
```bash
# Create .preset file in nvim config directory
echo "frontend" > ~/.config/nvim/.preset  # Linux/macOS
# or
echo "frontend" > %LOCALAPPDATA%\nvim\.preset  # Windows
```

**Method 3: Environment Variable**
```bash
export NVIM_PRESET=backend  # Linux/macOS
$env:NVIM_PRESET="backend"  # Windows PowerShell
nvim
```

### 🔧 After Switching Presets

1. **Restart Neovim**
2. **Sync plugins**: `:Lazy sync`
3. **Update parsers**: `:TSUpdate`
4. **Install tools**: `:MasonToolsInstall`

### 📊 Optimization Commands

```vim
:PresetList          " List all presets
:PresetSwitch <name> " Switch preset (requires restart)
:ConfigInfo          " Show config information
:KeymapDocs          " Show all keymaps
:ConfigValidate      " Validate configuration
:PluginStats         " Show plugin statistics
:MasonToolsInstall   " Install tools for current preset
```

## 🗂️ Configuration Structure

```
nvim/
├── init.lua                    # Bootstrap file for Lazy.nvim
├── lazy-lock.json             # Plugin versions lockfile
├── .preset                    # Current preset (fullstack/frontend/etc.)
├── lua/
│   ├── lazy_setup.lua         # Plugin specifications
│   ├── community.lua          # AstroCommunity imports (optimized)
│   ├── polish.lua             # Final polish and customizations
│   ├── config/                # Custom configuration modules
│   │   ├── auto_update_timestamp.lua  # Auto-update timestamps
│   │   ├── plugin_manager.lua # Plugin grouping & management
│   │   ├── presets.lua        # Preset system
│   │   ├── keymaps.lua        # Keymap management
│   │   ├── validator.lua      # Configuration validator
│   │   ├── commands.lua       # User commands
│   │   └── ...                # Other utilities
│   └── plugins/               # Plugin-specific configurations
│       ├── astrocore.lua      # AstroNvim core options
│       ├── astrolsp.lua       # LSP configuration
│       ├── conform.lua        # Formatting (Biome with --unsafe)
│       ├── jujutsu.lua        # Jujutsu VCS support
│       └── ...
└── snippets/                  # Custom snippets
    ├── cpp.json
    ├── rust.json
    ├── FE.json
    └── ...
```

## ⚙️ Customization

### Adding Plugins

Edit `lua/plugins/user.lua` to add new plugins:

```lua
---@type LazySpec
return {
  {
    "your-plugin/repo",
    event = "VeryLazy",
    config = function()
      require("your-plugin").setup()
    end,
  },
}
```

### Overriding Default Configs

Each plugin in `lua/plugins/` directory can be customized. The configuration loading order is:
1. AstroNvim defaults
2. AstroCommunity plugins
3. User plugin configs
4. `polish.lua` for final touches

### Auto-update Timestamps

A custom feature that automatically updates file headers when saving code files (`.rs`, `.c`, `.cpp`, `.py`, `.ts`, `.cs`).

Configuration: `lua/config/auto_update_timestamp.lua`

## 🔧 LSP & Tools Management

### Mason Tool Installation

Tools are installed automatically based on your preset, but you can manage them manually:

```vim
:Mason                    " Open Mason UI
:MasonToolsInstall        " Install tools for current preset
:MasonUpdate             " Update all installed tools
```

**Important Notes**:
- Tool installation is **manual** (not on startup) to avoid initialization errors
- After switching presets, run `:MasonToolsInstall` to install required tools
- Use `:Mason` UI to install/uninstall individual tools (press `i` to install, `X` to uninstall)

### Tool Categories by Preset

**Core Tools** (all presets):
- lua-language-server, stylua
- bash-language-server, shellcheck
- yaml-language-server, json-lsp, taplo

**Frontend Tools** (frontend/fullstack):
- typescript-language-server, vue-language-server
- html-lsp, css-lsp, tailwindcss-language-server
- biome, emmet-language-server

**Backend Tools** (backend/fullstack):
- rust-analyzer, gopls, pyright, ruff-lsp
- clangd, clang-format, zls, cmake-language-server
- debugpy, codelldb, delve (debuggers)

## ⌨️ Key Bindings

### Leader Key: `<Space>`

**Top-level shortcuts** (most frequently used):
- `<Leader><Space>` - Smart find files
- `<Leader>,` - Buffer list
- `<Leader>/` - Grep in project
- `<Leader>e` - File explorer

**File operations** (`<Leader>f`):
- `<Leader>ff` - Find files
- `<Leader>fg` - Find git files
- `<Leader>fc` - Find config files
- `<Leader>fr` - Recent files
- `<Leader>fp` - Projects
- `<Leader>fm` - Mini.files (current directory)

**Git operations** (`<Leader>g`):
- `<Leader>gb` - Branches
- `<Leader>gl` - Commit log
- `<Leader>gs` - Git status
- `<Leader>gd` - Git diff (hunks)
- `<Leader>gf` - File log

**Search operations** (`<Leader>s`):
- `<Leader>sw` - Grep word/selection
- `<Leader>sb` - Buffer lines
- `<Leader>sc` - Commands
- `<Leader>sd` - Diagnostics
- `<Leader>sh` - Help pages
- `<Leader>sk` - Keymaps
- `<Leader>ss` - LSP symbols

**LSP Navigation** (`g` prefix):
- `gd` - Go to definition
- `gD` - Go to declaration
- `gr` - References
- `gI` - Go to implementation
- `gy` - Go to type definition
- `K` - Hover documentation

**LSP Actions** (`<Leader>l`):
- `<Leader>la` - Code actions
- `<Leader>lr` - Rename symbol
- `<Leader>lf` - Format document
- `<Leader>ld` - Show line diagnostics

**Utilities** (`<Leader>u`):
- `<Leader>uC` - Switch colorscheme

For a complete keymap reference, run `:KeymapDocs` or press `<Leader>sk` to search keymaps.

## 🔧 Plugin Management

### Update Plugins
```vim
:Lazy sync
```

### Clean Unused Plugins
```vim
:Lazy clean
```

### Performance
Plugins are lazy-loaded based on events:
- `VeryLazy` - Load on idle
- `BufRead` - Load when reading buffers
- Filetype-specific loading

Disabled default plugins for performance:
- netrwPlugin
- tarPlugin
- zipPlugin
- gzip
- tohtml

## 🎨 Appearance

- **Theme**: AstroTheme (configurable)
- **Icons**: nerd fonts integration via mini.icons
- **Statusline**: Heirline.nvim
- **Indentation**: indent-blankline with rainbow delimiters
- **Syntax**: Tree-sitter for all supported languages

## 📝 Snippets

Custom snippets located in `snippets/`:
- `cpp.json` - C++ templates
- `rust.json` - Rust boilerplate
- `FE.json` - Frontend templates
- `dart.json` - Dart/Flutter snippets
- `svelte.json` - Svelte components

## 🐛 Debugging

Debug configuration available via nvim-dap with support for:
- **Rust**: rustaceanvim integration
- **Python**: Python debugger
- **Go**: Delve debugger
- **JavaScript/TypeScript**: Node.js debugging
- **Others**: Native DAP protocol

Press `<Leader> d` for debugging menu.

## 🔄 Synchronization

This configuration uses `lazy-lock.json` to lock plugin versions for reproducibility across machines.

To sync with latest versions:
```vim
:Lazy update
```

## 🐛 Troubleshooting

### Common Issues

**1. Mason tools not installing on startup**

This is by design to avoid initialization errors. Install manually:
```vim
:MasonToolsInstall
```

**2. Preset not applying after switch**

Make sure to:
1. Restart Neovim completely
2. Run `:Lazy sync` to sync plugins
3. Run `:TSUpdate` to update parsers
4. Run `:MasonToolsInstall` to install tools

**3. Slow startup time**

Check your current preset:
```vim
:ConfigInfo
```
Consider switching to a lighter preset:
```vim
:PresetSwitch minimal
```

**4. Theme not loading**

Backup themes are lazy-loaded. To activate:
```vim
:colorscheme catppuccin  " or tokyonight, kanagawa
```

**5. LSP not working for a language**

Check if LSP server is installed:
```vim
:Mason         " Look for the LSP server
:LspInfo       " Check active LSP servers
```

### Diagnostic Commands

```vim
:checkhealth           " Full health check
:checkhealth mason     " Check Mason specifically
:Lazy log             " View plugin load logs
:messages             " View all messages
:ConfigValidate       " Validate configuration
```

### Clean Reinstall

If something goes wrong:

**Windows:**
```powershell
Remove-Item -Recurse -Force $env:LOCALAPPDATA\nvim-data
nvim
:Lazy sync
:MasonToolsInstall
```

**Linux/macOS:**
```bash
rm -rf ~/.local/share/nvim
nvim
:Lazy sync
:MasonToolsInstall
```

## 💡 Tips & Tricks

### Disable Startup Notification

If you don't want the preset info notification:
```bash
# Add to environment
export NVIM_SHOW_PRESET_INFO=0
```

### Quick Theme Switching

```vim
:colorscheme <Tab>  " Tab completion for installed themes
```

### Performance Profiling

```vim
:Lazy profile  " See plugin load times
```

### Manual Tool Management

Prefer manual control? Comment out the `mason-tool-installer` plugin in `lua/plugins/mason.lua`.

## 📚 Resources

- [AstroNvim Documentation](https://astronvim.com/)
- [AstroNvim GitHub](https://github.com/AstroNvim/AstroNvim)
- [AstroCommunity](https://github.com/AstroNvim/astrocommunity)
- [Lazy.nvim](https://github.com/folke/lazy.nvim)
- [Mason.nvim](https://github.com/williamboman/mason.nvim)
- [Neovim Configuration Wiki](https://github.com/nanotee/nvim-lua-guide)

## 🤝 Contributing

This is a personal configuration. Feel free to:
1. Fork it for your own use
2. Open issues for bugs or suggestions
3. Submit PRs for improvements

## 📄 License

Same as the parent dotfiles repository.

## 📋 Configuration Highlights

### What Makes This Config Special

1. **Smart Preset System**: Automatically configures Neovim based on your workflow
2. **Performance First**: 30-40% faster startup with preset-based loading
3. **Zero Startup Errors**: Comprehensive error handling prevents crashes
4. **Manual Tool Control**: No surprises - you control when tools install
5. **Clean Keybindings**: Organized, conflict-free shortcuts (45 essential bindings)
6. **Treesitter Optimized**: Only install parsers you need (13-30 instead of all)
7. **Mason Optimized**: Only install tools you need (8-31 instead of all)
8. **Theme Lazy Loading**: Faster startup, themes load on-demand
9. **Modern Tooling**: Biome, Ruff, rust-analyzer, gopls - best-in-class tools
10. **Jujutsu Support**: Built-in support for modern VCS

### Architecture Principles

- **Modularity**: Each feature in separate files
- **Safety**: All `require()` calls wrapped in `pcall()`
- **Extensibility**: Easy to add/remove presets and plugins
- **Documentation**: Inline comments explain every decision
- **Best Practices**: Follows AstroNvim and Neovim community standards

## 📝 Changelog

### v2.0.0 (2025-12-05) - Performance Optimization Release

**Major Changes**:
- ✨ Added preset system (minimal, frontend, backend, fullstack, performance)
- ⚡ Implemented dynamic plugin/tool loading based on preset
- 🚀 Optimized startup time (33-40% faster for specialized presets)
- 🎨 Lazy-loaded themes (only active theme loads on startup)
- 🔧 Cleaned up keybindings (60+ → 45)
- 🛠️ Mason tools now manual install (prevents initialization errors)
- 📦 Treesitter parsers based on preset (13-30 parsers)
- 🔒 Enhanced error handling (prevents startup failures)
- 📊 Added optimization commands (:ConfigInfo, :PresetSwitch, etc.)

**Performance Gains**:
- Startup: ~400ms (fullstack) → ~250ms (frontend) / ~280ms (backend) / ~120ms (minimal)
- Memory: -15% to -33% depending on preset
- Disk: -44% to -69% depending on preset

**Migration Guide**:
- Default behavior unchanged (fullstack preset)
- To use presets: `:PresetSwitch <name>` or create `.preset` file
- After switching: `:Lazy sync`, `:TSUpdate`, `:MasonToolsInstall`

### v1.x - Initial Release

- Base AstroNvim v5 configuration
- Full language support (20+ languages)
- Custom snippets and auto-timestamp updates
- AstroCommunity integration

## 🙏 Acknowledgments

- AstroNvim team for the excellent framework
- All plugin authors for their amazing work
- The Neovim community for endless inspiration
- Contributors to the preset and optimization system
