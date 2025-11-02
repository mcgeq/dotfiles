---Cursor appearance configuration
local constants = require("config.constants")

return {
  -- Cursor style
  default_cursor_style = constants.CURSOR.STYLE,
  cursor_blink_ease_in = constants.CURSOR.BLINK_EASE_IN,
  cursor_blink_ease_out = constants.CURSOR.BLINK_EASE_OUT,
  cursor_blink_rate = constants.CURSOR.BLINK_RATE,
}

