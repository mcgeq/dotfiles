---Performance-related appearance configuration
local constants = require("config.constants")

return {
  -- Performance settings
  animation_fps = constants.PERFORMANCE.ANIMATION_FPS,
  max_fps = constants.PERFORMANCE.MAX_FPS,
  front_end = constants.PERFORMANCE.FRONT_END,
  webgpu_power_preference = constants.PERFORMANCE.WEBGPU_POWER_PREFERENCE,
}

