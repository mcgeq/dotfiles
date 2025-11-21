---Window position and size event handler
---Sets the initial window position and size on startup
local wezterm = require("wezterm")
local constants = require("config.constants")

local M = {}

function M.setup()
  wezterm.on("gui-startup", function(cmd)
    local tab, pane, window = wezterm.mux.spawn_window(cmd or {})
    
    -- 获取屏幕信息
    local screen = wezterm.gui.screens().active
    local screen_width = screen.width
    local screen_height = screen.height
    
    -- 获取窗口对象
    local gui_window = window:gui_window()
    
    -- 设置窗口大小（像素）
    local window_width = constants.WINDOW.SIZE.width
    local window_height = constants.WINDOW.SIZE.height
    
    -- 确保高度不超过屏幕高度
    if window_height > screen_height then
      window_height = screen_height
      wezterm.log_info("Window height adjusted to screen height: " .. screen_height)
    end
    
    -- 确保宽度不超过屏幕宽度
    if window_width > screen_width then
      window_width = screen_width
      wezterm.log_info("Window width adjusted to screen width: " .. screen_width)
    end
    
    -- 设置窗口大小
    gui_window:set_inner_size(window_width, window_height)
    
    -- 计算位置
    local x = constants.WINDOW.POSITION.x
    local y = constants.WINDOW.POSITION.y
    
    -- 如果 x 是负值，从右边计算
    if x < 0 then
      x = screen_width - window_width + x
    end
    
    -- 如果 y 是负值，从底部计算
    if y < 0 then
      y = screen_height - window_height + y
    end
    
    -- 确保窗口不会超出屏幕边界
    if x < 0 then
      x = 0
    end
    if y < 0 then
      y = 0
    end
    if x + window_width > screen_width then
      x = screen_width - window_width
    end
    if y + window_height > screen_height then
      y = screen_height - window_height
    end
    
    -- 设置窗口位置
    gui_window:set_position(x, y)
  end)
end

return M
