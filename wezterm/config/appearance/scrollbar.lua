---Scrollbar appearance configuration
local constants = require("config.constants")

return {
  -- Scrollbar settings
  enable_scroll_bar = constants.SCROLLBAR.ENABLED,
  min_scroll_bar_height = constants.SCROLLBAR.MIN_HEIGHT,
  colors = {
    scrollbar_thumb = constants.SCROLLBAR.THUMB_COLOR,
  },
}

