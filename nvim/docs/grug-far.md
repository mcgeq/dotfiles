# grug-far.nvim

`grug-far.nvim` is loaded on first use as the interactive project search/replace UI. Searches and history are isolated per workspace root.

## Active keymaps

Configured in `lua/plugins/search.lua`.

Current mappings:

- `<leader>sr`: open project replace
- `<leader>su`: resume the last grug-far search
- `<leader>sQ`: send the current grug-far results to quickfix
- `<leader>sW` in normal mode: replace current word
- `<leader>sW` in visual mode: replace current selection
- `<leader>sF`: replace only in current file
- `<leader>sF` in visual mode: replace the current selection only in the current file

## What it is good for

Use it when you want a reviewable replace workflow instead of raw `:%s` or external CLI tools.

Typical cases:

- rename a repeated string across a project
- update import paths
- replace API endpoints
- review regex-based replacements before applying them

## Current behavior

The base config uses ripgrep only, keeping search and replacement on the same engine:

```lua
require("grug-far").setup({ engine = "ripgrep", enabledEngines = { "ripgrep" } })
```

The plugin is mainly there for the workflow and keymaps, not for a heavily customized UI layer.

## Recommended usage

### Replace in project

1. Press `<leader>sr`
2. Fill in search text
3. Fill in replacement text
4. Review matches
5. Apply changes from the grug-far UI

If you close the panel and want to come back to the same search state, press `<leader>su`.

### Replace current word

1. Put the cursor on a symbol
2. Press `<leader>sW`
3. Narrow or replace from the panel

### Replace current selection

1. Select text in visual mode
2. Press `<leader>sW`
3. grug-far opens with the selection prefilled

### Replace only in current file

1. Press `<leader>sF`
2. grug-far opens in file-local mode

For a file-local replace from a visual selection:

1. Select text in visual mode
2. Press `<leader>sF`
3. grug-far opens scoped to the current file with the selection prefilled

### Send results to quickfix

1. Open a grug-far search
2. Press `<leader>sQ`
3. Use `[q` and `]q` to walk the matches from quickfix

## Why it stays

This plugin still earns its place because:

- it complements `snacks.picker.grep()` well
- it is much better than ad-hoc global replace for multi-file refactors
- it fits the current workflow without requiring a framework layer

`snacks` is the fast finder. `grug-far` is the safer interactive replace tool.

## Change or disable it

To disable it on this machine:

Edit `lua/user/private.lua`:

```lua
return {
  features = { search_replace = false },
}
```

The previous `private.features.spectre` switch is still honored unless `search_replace` is set explicitly.

## Related files

- `lua/pack/spec.lua`
- `lua/plugins/search.lua`
- `lua/user/private.example.lua`
