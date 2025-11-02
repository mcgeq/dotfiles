---Background appearance configuration
local wezterm = require("wezterm")
local constants = require("config.constants")

return {
  -- Background opacity
  window_background_opacity = constants.WINDOW.BACKGROUND_OPACITY,
  win32_system_backdrop = constants.APPEARANCE.WIN32_SYSTEM_BACKDROP,

  -- Background gradient
  window_background_gradient = {
    colors = {
      constants.COLORS.BACKGROUND_GRADIENT.COLOR_1,
      constants.COLORS.BACKGROUND_GRADIENT.COLOR_2,
    },
    orientation = {
      Linear = { angle = constants.COLORS.BACKGROUND_GRADIENT.ANGLE },
    },
  },

  -- Background layers
  background = {
    {
      source = { File = constants.PATHS.BACKDROP },
    },
    {
      source = { Color = constants.COLORS.BACKGROUND_OVERLAY.COLOR },
      height = "100%",
      width = "100%",
      opacity = constants.COLORS.BACKGROUND_OVERLAY.OPACITY,
    },
  },
}

