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

## ⚡ Configuration Optimization System

This configuration includes a powerful optimization system for better maintainability and flexibility:

### 🎯 Key Features

- **Plugin Manager** (`config/plugin_manager.lua`) - Grouped plugin management (15 categories)
- **Preset System** (`config/presets.lua`) - Quick switching between 5 scenarios
- **Keymap Manager** (`config/keymaps.lua`) - Centralized keymap management
- **Config Validator** (`config/validator.lua`) - Auto-check configuration on startup
- **User Commands** (`config/commands.lua`) - 6 convenient commands

### 📦 Available Presets

| Preset | Description | Use Case |
|--------|-------------|----------|
| `fullstack` | All plugins (default) | Full-stack development |
| `frontend` | ~70% plugins | JS/TS/Vue development |
| `backend` | ~70% plugins | Rust/Go/Python development |
| `minimal` | ~50% plugins | Quick editing, config files |
| `performance` | ~60% plugins | Low-spec machines |

### 🛠️ New Commands

```vim
:PresetList          " List all presets
:PresetSwitch <name> " Switch preset (requires restart)
:ConfigInfo          " Show config information
:KeymapDocs          " Show all keymaps
:ConfigValidate      " Validate configuration
:PluginStats         " Show plugin statistics
```

### 🔧 Quick Start

**Switch preset:**
```vim
:PresetSwitch minimal    " Switch to minimal
:PresetList              " View current preset
```

**Check config:**
```vim
:ConfigValidate   " Run validation
:ConfigInfo       " Show full info
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

## ⌨️ Key Bindings

### Leader Key: `<Space>`

Common shortcuts:
- `<Leader> p d` - Dashboard
- `<Leader> f f` - Find files
- `<Leader> f g` - Live grep
- `<Leader> e` - File explorer
- `<Leader> b b` - Buffer list
- `<Leader> c` - Close buffer
- `<Leader> t` - Terminal

### LSP
- `gD` - Go to declaration
- `gI` - Go to implementation
- `gd` - Go to definition
- `K` - Hover documentation
- `<Leader> ca` - Code actions
- `<Leader> cr` - Rename symbol

### Git
- `<Leader> tg` - Toggle Gitsigns
- `<Leader> g` - Git operations menu

For a complete keymap reference, press `?` in Neovim or check AstroNvim's [keybinding documentation](https://astronvim.com/Configuration/keymaps).

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

## 📚 Resources

- [AstroNvim Documentation](https://astronvim.com/)
- [AstroNvim GitHub](https://github.com/AstroNvim/AstroNvim)
- [AstroCommunity](https://github.com/AstroNvim/astrocommunity)
- [Lazy.nvim](https://github.com/folke/lazy.nvim)
- [Neovim Configuration Wiki](https://github.com/nanotee/nvim-lua-guide)

## 🤝 Contributing

This is a personal configuration. Feel free to:
1. Fork it for your own use
2. Open issues for bugs or suggestions
3. Submit PRs for improvements

## 📄 License

Same as the parent dotfiles repository.

## 🙏 Acknowledgments

- AstroNvim team for the excellent framework
- All plugin authors for their amazing work
- The Neovim community for endless inspiration
