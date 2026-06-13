---Right status bar event handler
---Displays date/time and battery status
---@see https://wezfurlong.org/wezterm/config/lua/wezterm/battery_info.html
local wezterm = require("wezterm")
local math = require("utils.math")

local M = {}

---Constants
M.separator_char = " ~ "

---Color scheme for right status elements
M.colors = {
  date_fg = "#7F82BB",
  date_bg = "#0F2536",
  battery_fg = "#BB49B3",
  battery_bg = "#0F2536",
  separator_fg = "#786D22",
  separator_bg = "#0F2536",
}

---Format items for wezterm status bar
M.cells = {}

---Add a status element to the cells array
---@param text string The text to display
---@param icon string The icon to display
---@param fg string Foreground color
---@param bg string Background color
---@param separate boolean Whether to add a separator after this element
function M.push(text, icon, fg, bg, separate)
  if not text or not icon or not fg or not bg then
    wezterm.log_error("right-status.push: Missing required parameters")
    return
  end

  table.insert(M.cells, { Foreground = { Color = fg } })
  table.insert(M.cells, { Background = { Color = bg } })
  table.insert(M.cells, { Attribute = { Intensity = "Bold" } })
  table.insert(M.cells, { Text = icon .. " " .. text .. " " })

  if separate then
    table.insert(M.cells, { Foreground = { Color = M.colors.separator_fg } })
    table.insert(M.cells, { Background = { Color = M.colors.separator_bg } })
    table.insert(M.cells, { Text = M.separator_char })
  end

  table.insert(M.cells, "ResetAttributes")
end

---Update date/time in status bar
function M.set_date()
  local ok, date = pcall(function()
    return wezterm.strftime(" %a %H:%M")
  end)

  if not ok then
    wezterm.log_error("right-status.set_date: Failed to format date")
    date = " --:-- "
  end

  M.push(date, "", M.colors.date_fg, M.colors.date_bg, true)
end

local last_battery_check = 0
local cached_charge = "N/A"
local cached_icon = "?"
local BATTERY_REFRESH_MS = 30000

---Update battery status in status bar
---@see https://wezfurlong.org/wezterm/config/lua/wezterm/battery_info.html
function M.set_battery()
  local now = wezterm.time_now()
  if now - last_battery_check < BATTERY_REFRESH_MS then
    M.push(cached_charge, cached_icon, M.colors.battery_fg, M.colors.battery_bg, false)
    return
  end
  last_battery_check = now

  local discharging_icons = { "󰂃", "󰁻", "󰁼", "󰁽", "󰁾", "󰁿", "󰂀", "󰂁", "󰂂", "󰁹" }
  local charging_icons = { "󰢜", "󰂆", "󰂇", "󰂈", "󰢝", "󰂉", "󰢞", "󰂊", "󰂋", "󰂅" }

  cached_charge = "N/A"
  cached_icon = "?"

  local ok, battery_info = pcall(function()
    return wezterm.battery_info()
  end)

  if ok and battery_info then
    for _, b in ipairs(battery_info) do
      local idx = math.clamp(math.round(b.state_of_charge * 10), 1, 10)
      cached_charge = string.format("%.0f%%", b.state_of_charge * 100)

      if b.state == "Charging" then
        cached_icon = charging_icons[idx] or charging_icons[1]
      else
        cached_icon = discharging_icons[idx] or discharging_icons[1]
      end
      break
    end
  else
    wezterm.log_warn("right-status.set_battery: Failed to get battery info")
  end

  M.push(cached_charge, cached_icon, M.colors.battery_fg, M.colors.battery_bg, false)
end

---Setup the right status bar event handler
function M.setup()
  wezterm.on("update-right-status", function(window, pane)
    if not window then
      wezterm.log_error("right-status.setup: window is nil")
      return
    end

    -- Reset cells for this update
    M.cells = {}

    -- Update status elements
    M.set_date()
    M.set_battery()

    -- Set the status bar
    local ok, err = pcall(function()
      window:set_right_status(wezterm.format(M.cells))
    end)

    if not ok then
      wezterm.log_error("right-status.setup: Failed to set right status: " .. tostring(err))
    end
  end)
end

return M
