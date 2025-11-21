---Constants used throughout the configuration
---This module centralizes all magic values for better maintainability

local wezterm = require("wezterm")

local M = {}

-- ============================================================================
-- Window & Layout Constants
-- ============================================================================
M.WINDOW = {
  INITIAL_COLS = 90,
  INITIAL_ROWS = 48,
  PADDING = {
    left = 5,
    right = 10,
    top = 12,
    bottom = 7,
  },
  BACKGROUND_OPACITY = 1.0,
  CLOSE_CONFIRMATION = "NeverPrompt",
  -- 窗口大小配置 (像素)
  SIZE = {
    width = 1200,   -- 窗口宽度（像素）
    height = 900,   -- 窗口高度（像素），自动限制不超过屏幕高度
  },
  -- 窗口位置配置 (居右)
  -- x: 距离屏幕左边的像素距离，使用负值表示距离右边的距离
  -- y: 距离屏幕顶部的像素距离
  POSITION = {
    x = -10,  -- 距离屏幕右边 10 像素
    y = 50,   -- 距离屏幕顶部 50 像素
  },
}

-- ============================================================================
-- Tab Bar Constants
-- ============================================================================
M.TAB_BAR = {
  ENABLED = true,
  AT_BOTTOM = false,
  HIDE_IF_ONLY_ONE = false,
  USE_FANCY = true,
  MAX_WIDTH = 25,
  SHOW_INDEX = true,
  SWITCH_TO_LAST_ACTIVE = true,
}

-- ============================================================================
-- Cursor Constants
-- ============================================================================
M.CURSOR = {
  STYLE = "BlinkingBar",
  BLINK_EASE_IN = "Constant",
  BLINK_EASE_OUT = "Constant",
  BLINK_RATE = 700,
}

-- ============================================================================
-- Scrollbar Constants
-- ============================================================================
M.SCROLLBAR = {
  ENABLED = false,
  MIN_HEIGHT = "3cell",
  THUMB_COLOR = "#34354D",
}

-- ============================================================================
-- Performance Constants
-- ============================================================================
M.PERFORMANCE = {
  ANIMATION_FPS = 60,
  MAX_FPS = 60,
  FRONT_END = "WebGpu",
  WEBGPU_POWER_PREFERENCE = "HighPerformance",
}

-- ============================================================================
-- Appearance Constants
-- ============================================================================
M.APPEARANCE = {
  TERM = "xterm-256color",
  COLOR_SCHEME = "Gruvbox dark, medium (base16)",
  WINDOW_DECORATIONS = "INTEGRATED_BUTTONS|RESIZE",
  INTEGRATED_TITLE_BUTTON_STYLE = "Windows",
  INTEGRATED_TITLE_BUTTON_COLOR = "auto",
  INTEGRATED_TITLE_BUTTON_ALIGNMENT = "Right",
  WIN32_SYSTEM_BACKDROP = "Acrylic",
}

-- ============================================================================
-- Color Constants
-- ============================================================================
M.COLORS = {
  -- Window frame colors
  WINDOW_FRAME = {
    ACTIVE_TITLEBAR_BG = "#0F2536",
    INACTIVE_TITLEBAR_BG = "#0F2536",
  },
  -- Background gradient
  BACKGROUND_GRADIENT = {
    COLOR_1 = "#1D261B",
    COLOR_2 = "#261A25",
    ANGLE = -45.0,
  },
  -- Background overlay
  BACKGROUND_OVERLAY = {
    COLOR = "#1A1B26",
    OPACITY = 0.95,
  },
  -- Inactive pane
  INACTIVE_PANE_HSB = {
    saturation = 1.0,
    brightness = 1.0,
  },
}

-- ============================================================================
-- Path Constants
-- ============================================================================
M.PATHS = {
  BACKDROP = wezterm.config_dir .. "/backdrops/space.jpg",
}

return M

