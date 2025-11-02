---New tab button click event handler
---Handles left and right clicks on the new tab button
local wezterm = require("wezterm")
local act = wezterm.action

local M = {}

---Setup the new tab button click event handler
function M.setup()
  wezterm.on("new-tab-button-click", function(window, pane, button, default_action)
    if not window then
      wezterm.log_error("new-tab-button.setup: window is nil")
      return false
    end

    if not button then
      wezterm.log_error("new-tab-button.setup: button is nil")
      return false
    end

    -- Left click: perform default action (create new tab)
    if button == "Left" and default_action then
      local ok, err = pcall(function()
        window:perform_action(default_action, pane)
      end)

      if not ok then
        wezterm.log_error("new-tab-button.setup: Failed to perform default action: " .. tostring(err))
      end
      return false
    end

    -- Right click: show launcher with menu
    if button == "Right" then
      local ok, err = pcall(function()
        window:perform_action(
          act.ShowLauncherArgs({
            title = "󰈲 Select/Search:",
            flags = "FUZZY|LAUNCH_MENU_ITEMS|DOMAINS",
          }),
          pane
        )
      end)

      if not ok then
        wezterm.log_error("new-tab-button.setup: Failed to show launcher: " .. tostring(err))
      end
      return false
    end

    -- Unhandled button click
    return false
  end)
end

return M
