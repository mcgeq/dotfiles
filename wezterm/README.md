# Wezterm Configuration

This is a modular and maintainable Wezterm configuration that separates concerns into logical modules for better extensibility and maintainability.

## Directory Structure

```
wezterm/
├── wezterm.lua              # Main entry point
├── config/                   # Configuration modules
│   ├── init.lua             # Config class with merge/append utilities
│   ├── constants.lua        # Centralized constants (colors, sizes, etc.)
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
└── colors/                   # Color schemes
    └── custom.lua
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

## How to Modify

### Adding a New Key Binding
1. Open `config/bindings/keys.lua`
2. Find the appropriate function (e.g., `tab_bindings()`, `pane_bindings()`)
3. Add your binding to the corresponding array
4. Or create a new function for a new category

### Changing Appearance Constants
1. Open `config/constants.lua`
2. Modify the relevant constant (e.g., `WINDOW.INITIAL_COLS`, `COLORS.WINDOW_FRAME`)
3. Changes will automatically apply to all modules using these constants

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
