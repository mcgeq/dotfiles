local wezterm = require('wezterm')
local colors = require('colors.custom')
-- local fonts = require('config.fonts')

return {
   term = "xterm-256color",
   animation_fps = 60,
   max_fps = 60,
   front_end = 'WebGpu',
   webgpu_power_preference = 'HighPerformance',

   -- color scheme
   -- colors = colors,
   color_scheme = "Gruvbox dark, medium (base16)",

   -- background
   window_background_opacity = 1.0,
   win32_system_backdrop = 'Acrylic',
   window_background_gradient = {
    colors = { "#1D261B", "#261A25" },
    orientation = { Linear = { angle = -45.0 } },
   },
   background = {
    {
       source = { File = wezterm.config_dir .. '/backdrops/space.jpg' },
    },
     {
        source = { Color = "#1A1B26" },
        height = '100%',
        width = '100%',
        opacity = 0.95,
     },
   },

   -- scrollbar
   enable_scroll_bar = false,
   min_scroll_bar_height = "3cell",
   colors = {
      scrollbar_thumb = '#34354D',
   },

   -- tab bar
   -- 启用选项卡
   enable_tab_bar = true,
   -- 选项卡位于底部
   -- tab_bar_at_bottom = true,
   -- 一个选项卡时隐藏
--   hide_tab_bar_if_only_one_tab = true,
   use_fancy_tab_bar = true,
   tab_max_width = 25,
   show_tab_index_in_tab_bar = true,
   switch_to_last_active_tab_when_closing_tab = true,

   -- cursor
   default_cursor_style = "BlinkingBar",
   cursor_blink_ease_in = "Constant",
   cursor_blink_ease_out = "Constant",
   cursor_blink_rate = 700,

   -- window
   window_decorations = "INTEGRATED_BUTTONS|RESIZE",
   integrated_title_button_style = "Windows",
   integrated_title_button_color = "auto",
   integrated_title_button_alignment = "Right",
   initial_cols = 90,
   initial_rows = 48,
   window_padding = {
      left = 5,
      right = 10,
      top = 12,
      bottom = 7,
   },
   window_close_confirmation = 'NeverPrompt',
   window_frame = {
      active_titlebar_bg = '#0F2536',
      inactive_titlebar_bg = '#0F2536',
      -- font = fonts.font,
      -- font_size = fonts.font_size,
   },
   inactive_pane_hsb = { saturation = 1.0, brightness = 1.0 },
}
