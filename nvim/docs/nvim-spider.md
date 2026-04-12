# nvim-spider

`nvim-spider` is active in the current config and replaces the default word motions with smarter subword-aware movement.

## Active behavior

Configured in [lua/plugins/editor.lua](/d:/config/dotfiles/nvim/lua/plugins/editor.lua).

Current mappings:

- `w`: next smart word start
- `e`: next smart word end
- `b`: previous smart word start
- `ge`: previous smart word end

These mappings are active in:

- normal mode
- visual mode
- operator-pending mode

That means commands like `dw`, `cw`, `ve`, and `d2w` keep working, but they skip punctuation more naturally than stock Vim.

## Current setup

The base config enables:

```lua
require("spider").setup({
  skipInsignificantPunctuation = true,
  subwordMovement = true,
  consistentOperatorPending = false,
})
```

This keeps movement smarter without aggressively changing all operator-pending semantics.

## Why keep it

It is lightweight and improves daily navigation in:

- JavaScript / TypeScript
- Vue / JSX / TSX
- Rust / Zig / Go
- Lua / Python

Examples:

- `user.getName().toUpperCase()` takes fewer `w` presses
- `foo_bar_baz` and `fooBarBaz` feel more consistent
- `dw` is less likely to stop on punctuation noise

## Change or disable it

If you want to override or remove it without editing the base config:

1. Disable the plugin in [lua/user/init.lua](/d:/config/dotfiles/nvim/lua/user/init.lua):

```lua
return {
  pack = {
    disable = { "nvim-spider" },
  },
}
```

2. Or remap the motions in [lua/user/keymaps.lua](/d:/config/dotfiles/nvim/lua/user/keymaps.lua).

Example:

```lua
return function(map)
  map.map({ "n", "o", "x" }, "w", "w", "Builtin forward word")
  map.map({ "n", "o", "x" }, "e", "e", "Builtin end word")
  map.map({ "n", "o", "x" }, "b", "b", "Builtin backward word")
  map.map({ "n", "o", "x" }, "ge", "ge", "Builtin backward end word")
end
```

## Related files

- [lua/pack/spec.lua](/d:/config/dotfiles/nvim/lua/pack/spec.lua)
- [lua/plugins/editor.lua](/d:/config/dotfiles/nvim/lua/plugins/editor.lua)
- [lua/user/keymaps.lua](/d:/config/dotfiles/nvim/lua/user/keymaps.lua)
