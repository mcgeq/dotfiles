# User LSP

Add one Lua file per extra language server.

Example:

```lua
return {
  name = "clangd",
  ensure_installed = true,
  config = {},
}
```

Vue / TS example:

```lua
return {
  name = "vue_ls",
  ensure_installed = true,
  config = {
    settings = {
      vue = {
        inlayHints = {
          missingProps = true,
        },
      },
    },
  },
}
```
