---Color scheme and appearance configuration
local constants = require("config.constants")

return {
  -- Terminal type
  term = constants.APPEARANCE.TERM,

  -- Color scheme
  color_scheme = constants.APPEARANCE.COLOR_SCHEME,

  -- Inactive pane appearance
  inactive_pane_hsb = constants.COLORS.INACTIVE_PANE_HSB,
}

