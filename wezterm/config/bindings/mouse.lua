---Mouse bindings configuration
local act = require("wezterm").action

return {
  -- Ctrl-click will open the link under the mouse cursor
  {
    event = { Up = { streak = 1, button = "Left" } },
    mods = "CTRL",
    action = act.OpenLinkAtMouseCursor,
  },
  -- Move mouse will only select text and not copy text to clipboard
  {
    event = { Down = { streak = 1, button = "Left" } },
    mods = "NONE",
    action = act.SelectTextAtMouseCursor("Cell"),
  },
  {
    event = { Up = { streak = 1, button = "Left" } },
    mods = "NONE",
    action = act.ExtendSelectionToMouseCursor("Cell"),
  },
  {
    event = { Drag = { streak = 1, button = "Left" } },
    mods = "NONE",
    action = act.ExtendSelectionToMouseCursor("Cell"),
  },
  -- Triple Left click will select a line
  {
    event = { Down = { streak = 3, button = "Left" } },
    mods = "NONE",
    action = act.SelectTextAtMouseCursor("Line"),
  },
  {
    event = { Up = { streak = 3, button = "Left" } },
    mods = "NONE",
    action = act.SelectTextAtMouseCursor("Line"),
  },
  -- Double Left click will select a word
  {
    event = { Down = { streak = 2, button = "Left" } },
    mods = "NONE",
    action = act.SelectTextAtMouseCursor("Word"),
  },
  {
    event = { Up = { streak = 2, button = "Left" } },
    mods = "NONE",
    action = act.SelectTextAtMouseCursor("Word"),
  },
  -- Turn on the mouse wheel to scroll the screen
  {
    event = { Down = { streak = 1, button = { WheelUp = 1 } } },
    mods = "NONE",
    action = act.ScrollByCurrentEventWheelDelta,
  },
  {
    event = { Down = { streak = 1, button = { WheelDown = 1 } } },
    mods = "NONE",
    action = act.ScrollByCurrentEventWheelDelta,
  },
}

