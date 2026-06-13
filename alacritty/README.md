# Alacritty Configuration

GPU-accelerated terminal emulator config with JetBrainsMono Nerd Font, WSL Arch integration, and Catppuccin Mocha theme.

## Files

```
alacritty/
├── alacritty.toml       # Main config: window, font, cursor, scrolling, shell, env
├── colors.toml           # Catppuccin Mocha color scheme
├── keybindings.toml      # Custom keybindings
├── README.md             # This file
└── README.zh.md          # Chinese documentation
```

## Highlights

- **Font**: JetBrainsMono Nerd Font, 12pt
- **Shell**: WSL Arch (`wsl -d Arch -e fish`)
- **Theme**: Catppuccin Mocha with custom background (`#1f1f28`), matching Wezterm colors
- **Opacity**: 0.95 with blur (Windows 11 only)
- **Window**: Auto-theme (`decorations_theme_variant = "System"`), dynamic padding
- **Cursor**: Beam style, blinking, non-hollow when unfocused
- **Scroll**: 10000 lines, 3x multiplier
- **OSC 52**: Clipboard sync support
- **TERM**: `alacritty` (official recommended value for true color support)
- **Modular**: Colors and keybindings as separate import files

## Key Bindings

### General

| Keys | Action |
|------|--------|
| `Ctrl+Shift+N` | Create new window |
| `F11` | Toggle fullscreen |
| `Ctrl+F11` | Toggle maximized |
| `Ctrl+Shift+H` | Hide window |
| `Ctrl+Shift+M` | Minimize window |
| `Ctrl+Shift+Q` | Quit Alacritty |

### Copy / Paste / Selection

| Keys | Action |
|------|--------|
| `Ctrl+Shift+C` | Copy (built-in default) |
| `Ctrl+Shift+V` | Paste (built-in default) |
| `Ctrl+Right click` | Paste |
| `Escape` | Clear selection |

### Search

| Keys | Action |
|------|--------|
| `Ctrl+Shift+F` | Search forward |
| `Ctrl+Shift+B` | Search backward |
| `Enter` (search mode) | Confirm search |
| `Escape` (search mode) | Cancel search |
| `U` (search mode) | Clear search |

### Scroll

| Keys | Action |
|------|--------|
| `PageUp` | Scroll page up |
| `PageDown` | Scroll page down |
| `Home` | Scroll to top |
| `End` | Scroll to bottom |
| `Ctrl+L` | Clear history / scrollback |

### Hints (URLs)

| Keys | Action |
|------|--------|
| `Ctrl+Shift+O` | Copy URL under cursor to clipboard |

## Theme

Based on Catppuccin Mocha, with adjustments to match Wezterm config:

- **Background**: `#1f1f28` (slightly lighter than standard `#1e1e2e`)
- **Foreground**: `#cdd6f4`
- **Cursor**: Rosewater (`#f5e0dc`)
- **Selection**: Surface2 (`#585b70`)
- **Hints**: Yellow bg (`#f9e2af`) / Red bg (`#f38ba8`)
