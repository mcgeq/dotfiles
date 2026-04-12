# nvim-spectre

`nvim-spectre` is active in the current config as the project-wide search and replace UI.

## Active keymaps

Configured in [lua/plugins/search.lua](/d:/config/dotfiles/nvim/lua/plugins/search.lua).

Current mappings:

- `<leader>sr`: open project replace
- `<leader>sW` in normal mode: replace current word
- `<leader>sW` in visual mode: replace current selection
- `<leader>sF`: replace only in current file

## What it is good for

Use it when you want a reviewable replace workflow instead of raw `:%s` or external CLI tools.

Typical cases:

- rename a repeated string across a project
- update import paths
- replace API endpoints
- review regex-based replacements before applying them

## Current behavior

The base config keeps it intentionally simple:

```lua
require("spectre").setup({})
```

The plugin is mainly there for the workflow and keymaps, not for a heavily customized UI layer.

## Recommended usage

### Replace in project

1. Press `<leader>sr`
2. Fill in search text
3. Fill in replacement text
4. Review matches
5. Apply changes from the Spectre UI

### Search current word

1. Put the cursor on a symbol
2. Press `<leader>sW`
3. Narrow or replace from the panel

### Search current selection

1. Select text in visual mode
2. Press `<leader>sW`
3. Spectre opens with the selection prefilled

### Search only in current file

1. Press `<leader>sF`
2. Spectre opens in file-local mode

## Why it stays

This plugin still earns its place because:

- it complements `snacks.picker.grep()` well
- it is much better than ad-hoc global replace for multi-file refactors
- it fits the current workflow without requiring a framework layer

`snacks` is the fast finder. `spectre` is the safer interactive replace tool.

## Change or disable it

If you want to remove it:

Edit [lua/user/init.lua](/d:/config/dotfiles/nvim/lua/user/init.lua):

```lua
return {
  pack = {
    disable = { "nvim-spectre" },
  },
}
```

If you only want different keymaps, override them in [lua/user/keymaps.lua](/d:/config/dotfiles/nvim/lua/user/keymaps.lua).

## Related files

- [lua/pack/spec.lua](/d:/config/dotfiles/nvim/lua/pack/spec.lua)
- [lua/plugins/search.lua](/d:/config/dotfiles/nvim/lua/plugins/search.lua)
- [lua/user/init.lua](/d:/config/dotfiles/nvim/lua/user/init.lua)
