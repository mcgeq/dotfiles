---Window appearance configuration
local wezterm = require("wezterm")
local constants = require("config.constants")

return {
  -- Window decorations
  window_decorations = constants.APPEARANCE.WINDOW_DECORATIONS,
  integrated_title_button_style = constants.APPEARANCE.INTEGRATED_TITLE_BUTTON_STYLE,
  integrated_title_button_color = constants.APPEARANCE.INTEGRATED_TITLE_BUTTON_COLOR,
  integrated_title_button_alignment = constants.APPEARANCE.INTEGRATED_TITLE_BUTTON_ALIGNMENT,

  -- Window size and padding
  initial_cols = constants.WINDOW.INITIAL_COLS,
  initial_rows = constants.WINDOW.INITIAL_ROWS,
  window_padding = {
    left = constants.WINDOW.PADDING.left,
    right = constants.WINDOW.PADDING.right,
    top = constants.WINDOW.PADDING.top,
    bottom = constants.WINDOW.PADDING.bottom,
  },

  -- Window behavior
  window_close_confirmation = constants.WINDOW.CLOSE_CONFIRMATION,

  -- Window frame
  window_frame = {
    active_titlebar_bg = constants.COLORS.WINDOW_FRAME.ACTIVE_TITLEBAR_BG,
    inactive_titlebar_bg = constants.COLORS.WINDOW_FRAME.INACTIVE_TITLEBAR_BG,
  },
}

