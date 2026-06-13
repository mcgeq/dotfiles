# Wezterm Configuration

This is a modular and maintainable Wezterm configuration that separates concerns into logical modules for better extensibility and maintainability.

## Directory Structure

```
wezterm/
├── wezterm.lua              # Main entry point
├── user.lua.example         # Template for personal overrides (copy → user.lua)
├── user.lua                 # Personal overrides (gitignored, optional)
├── config/                   # Configuration modules
│   ├── init.lua             # Config class with merge/append utilities
│   ├── constants.lua        # Centralized constants (colors, sizes, etc.) + user.lua merge
│   ├── appearance.lua       # Appearance configuration aggregator
│   │   └── appearance/      # Appearance sub-modules
│   │       ├── window.lua
│   │       ├── background.lua
│   │       ├── tab_bar.lua
│   │       ├── cursor.lua
│   │       ├── scrollbar.lua
│   │       ├── performance.lua
│   │       └── colors.lua
│   ├── bindings.lua         # Bindings configuration aggregator
│   │   └── bindings/        # Binding sub-modules
│   │       ├── keys.lua     # Keyboard bindings (organized by function)
│   │       ├── key_tables.lua
│   │       └── mouse.lua
│   ├── domains.lua          # SSH/WSL/Unix domain configurations
│   ├── fonts.lua            # Font configurations
│   ├── general.lua          # General behavior settings
│   └── launch.lua           # Shell launch configurations
├── events/                   # Event handlers
│   ├── right-status.lua     # Right status bar
│   ├── tab-title.lua        # Tab title formatting
│   └── new-tab-button.lua   # New tab button behavior
├── utils/                    # Utility functions
│   ├── platform.lua         # Platform detection
│   ├── shells.lua            # Shell detection
│   └── math.lua             # Math utilities

```

## Design Principles

### 1. Modularity
- Each configuration aspect is separated into its own module
- Related configurations are grouped in subdirectories
- Easy to locate and modify specific features

### 2. Constants Management
- All magic values (colors, sizes, etc.) are centralized in `config/constants.lua`
- Easy to maintain and update appearance settings
- No hardcoded values scattered across files

### 3. Config Class Enhancement
The `Config` class provides:
- `append()`: Shallow merge (for non-conflicting options)
- `merge()`: Deep merge (for nested configurations like colors)
- `get_options()`: Get a copy of current options (for debugging)
- Better error handling and duplicate detection

### 4. Functional Organization
- Bindings are organized by function (tabs, panes, fonts, etc.)
- Appearance settings are grouped by component (window, tab_bar, cursor, etc.)
- Each module has a single responsibility

## Key Bindings

Modifier legend (Windows): `ALT` = Super, `ALT|CTRL` = Super+Ctrl. On macOS `SUPER` = Cmd.

### General & Copy/Paste

| Keys | Action |
|------|--------|
| `F1` | Activate copy mode |
| `F2` | Command palette |
| `F3` | Launcher |
| `F4` | Tab navigator |
| `F11` | Toggle full screen |
| `F12` | Debug overlay |
| `ALT-f` | Search |
| `CTRL-SHIFT-c` | Copy to clipboard |
| `CTRL-SHIFT-v` | Paste from clipboard |

### Tabs

| Keys | Action |
|------|--------|
| `ALT-t` | Spawn default domain tab |
| `ALT|CTRL-t` | Spawn WSL tab (see `config/constants.lua` → `M.WSL` to customize distro/user/path) |
| `ALT-[` / `ALT-]` | Activate previous/next tab |
| `ALT|CTRL-[` / `ALT|CTRL-]` | Move tab left/right |
| `ALT-q` | Close current pane |
| `ALT|CTRL-q` | Close current tab |
| `CTRL-SHIFT-R` | Rename current tab |

### Panes

| Keys | Action |
|------|--------|
| `ALT|CTRL-/` | Split vertically |
| `ALT|CTRL-\` | Split horizontally |
| `ALT|CTRL--` | Close current pane (with confirm) |
| `ALT|CTRL-z` | Toggle pane zoom |
| `ALT|CTRL-h/j/k/l` | Navigate pane left/down/up/right |
| `ALT|CTRL-arrows` | Resize pane |

### Font

| Keys | Action |
|------|--------|
| `ALT-UpArrow` | Increase font size |
| `ALT-DownArrow` | Decrease font size |
| `ALT-r` | Reset font size |

### Leader Key Tables

Press `LEADER` (default `CTRL-a`) then:

| Sequence | Action |
|----------|--------|
| `f` | Enter font resize mode (1s timeout) |
| `p` | Enter pane resize mode (1s timeout) |

**Font resize mode** (auto-exits after 1s):

| Keys | Action |
|------|--------|
| `k` / `j` | Increase / decrease font size |
| `r` | Reset font size |
| `Escape` / `q` | Exit |

**Pane resize mode** (auto-exits after 1s):

| Keys | Action |
|------|--------|
| `k` / `j` | Resize up / down |
| `h` / `l` | Resize left / right |
| `Escape` / `q` | Exit |

### Mouse

| Gesture | Action |
|---------|--------|
| `CTRL` + Left click | Open link under cursor |
| Left click / drag | Select cells |
| Double left click | Select word |
| Triple left click | Select line |
| Scroll wheel | Scroll output |

## How to Modify

### Adding a New Key Binding
1. Open `config/bindings/keys.lua`
2. Find the appropriate table constant (e.g., `TAB_BINDINGS`, `PANE_BINDINGS`)
3. Add your binding to the corresponding table
4. Or create a new table constant for a new category and include it in `combine_bindings()`

### Changing Appearance Constants
1. Open `config/constants.lua`
2. Modify the relevant constant (e.g., `WINDOW.INITIAL_COLS`, `COLORS.WINDOW_FRAME`)
3. Changes will automatically apply to all modules using these constants

### Personal Overrides (user.lua)

Create `user.lua` (gitignored) in the wezterm root to override any constant without touching the shared config:

```lua
local M = {}
M.WSL = {
  DISTRIBUTION = "Ubuntu",
  USERNAME = "yourname",
  DEFAULT_CWD = "/home/yourname",
  DEFAULT_PROG = { "zsh" },
}
return M
```

You can override any field from `config/constants.lua` — window size, color scheme, WSL domain, etc. See `user.lua.example` for all options.

### Adding a New Appearance Module
1. Create a new file in `config/appearance/` (e.g., `new_feature.lua`)
2. Return a table with your configuration
3. Import and merge it in `config/appearance.lua`

### Extending Event Handlers
1. Event handlers are in `events/` directory
2. Each handler module exports a `setup()` function
3. Handlers are registered in `wezterm.lua`

## Benefits of This Structure

1. **Maintainability**: Clear separation of concerns makes it easy to find and modify specific features
2. **Extensibility**: Easy to add new features without touching existing code
3. **Readability**: Related configurations are grouped together
4. **Reusability**: Constants can be reused across modules
5. **Testability**: Each module can be tested independently
6. **Documentation**: Clear structure serves as implicit documentation
