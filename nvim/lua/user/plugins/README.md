# User Plugins

Add one Lua file per custom plugin or plugin group.

Example:

```lua
return {
  spec = {
    src = "https://github.com/stevearc/oil.nvim",
    name = "oil.nvim",
  },
  setup = function()
    require("oil").setup()
  end,
}
```
