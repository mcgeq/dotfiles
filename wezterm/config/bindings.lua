---Bindings configuration module
---Combines keyboard, key table, and mouse bindings
local keys_config = require("config.bindings.keys")
local key_tables_config = require("config.bindings.key_tables")
local mouse_config = require("config.bindings.mouse")

return {
  -- General binding settings
  disable_default_key_bindings = true,
  disable_default_mouse_bindings = true,
  leader = { key = "Space", mods = "CTRL|SHIFT" },

  -- Keyboard bindings
  keys = keys_config.keys,

  -- Key tables for modal bindings
  key_tables = key_tables_config,

  -- Mouse bindings
  mouse_bindings = mouse_config,
}
