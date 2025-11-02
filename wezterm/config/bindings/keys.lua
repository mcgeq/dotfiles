---Keyboard bindings configuration
local wezterm = require("wezterm")
local platform = require("utils.platform")()
local act = wezterm.action

local mod = {}

if platform.is_mac then
  mod.SUPER = "SUPER"
  mod.SUPER_REV = "SUPER|CTRL"
elseif platform.is_win then
  mod.SUPER = "ALT" -- to not conflict with Windows key shortcuts
  mod.SUPER_REV = "ALT|CTRL"
end

---Miscellaneous/useful bindings
local function misc_bindings()
  return {
    { key = "F1", mods = "NONE", action = "ActivateCopyMode" },
    { key = "F2", mods = "NONE", action = act.ActivateCommandPalette },
    { key = "F3", mods = "NONE", action = act.ShowLauncher },
    { key = "F4", mods = "NONE", action = act.ShowTabNavigator },
    { key = "F11", mods = "NONE", action = act.ToggleFullScreen },
    { key = "F12", mods = "NONE", action = act.ShowDebugOverlay },
    { key = "f", mods = mod.SUPER, action = act.Search({ CaseInSensitiveString = "" }) },
  }
end

---Copy/paste bindings
local function copy_paste_bindings()
  return {
    { key = "c", mods = "CTRL|SHIFT", action = act.CopyTo("Clipboard") },
    { key = "v", mods = "CTRL|SHIFT", action = act.PasteFrom("Clipboard") },
  }
end

---Tab-related bindings
local function tab_bindings()
  return {
    -- tabs: spawn+close
    { key = "t", mods = mod.SUPER, action = act.SpawnTab("DefaultDomain") },
    { key = "t", mods = mod.SUPER_REV, action = act.SpawnTab({ DomainName = "WSL:Arch" }) },
    { key = "q", mods = mod.SUPER_REV, action = act.CloseCurrentTab({ confirm = false }) },

    -- tabs: navigation
    { key = "[", mods = mod.SUPER, action = act.ActivateTabRelative(-1) },
    { key = "]", mods = mod.SUPER, action = act.ActivateTabRelative(1) },
    { key = "[", mods = mod.SUPER_REV, action = act.MoveTabRelative(-1) },
    { key = "]", mods = mod.SUPER_REV, action = act.MoveTabRelative(1) },
  }
end

---Window-related bindings
local function window_bindings()
  return {
    { key = "n", mods = mod.SUPER, action = act.SpawnWindow },
  }
end

---Pane-related bindings
local function pane_bindings()
  return {
    -- panes: split panes
    {
      key = [[/]],
      mods = mod.SUPER_REV,
      action = act.SplitVertical({ domain = "CurrentPaneDomain" }),
    },
    {
      key = [[\]],
      mods = mod.SUPER_REV,
      action = act.SplitHorizontal({ domain = "CurrentPaneDomain" }),
    },
    {
      key = [[-]],
      mods = mod.SUPER_REV,
      action = act.CloseCurrentPane({ confirm = true }),
    },

    -- panes: zoom+close pane
    { key = "z", mods = mod.SUPER_REV, action = act.TogglePaneZoomState },
    { key = "q", mods = mod.SUPER, action = act.CloseCurrentPane({ confirm = false }) },

    -- panes: navigation
    { key = "k", mods = mod.SUPER_REV, action = act.ActivatePaneDirection("Up") },
    { key = "j", mods = mod.SUPER_REV, action = act.ActivatePaneDirection("Down") },
    { key = "h", mods = mod.SUPER_REV, action = act.ActivatePaneDirection("Left") },
    { key = "l", mods = mod.SUPER_REV, action = act.ActivatePaneDirection("Right") },

    -- panes: resize
    { key = "UpArrow", mods = mod.SUPER_REV, action = act.AdjustPaneSize({ "Up", 1 }) },
    { key = "DownArrow", mods = mod.SUPER_REV, action = act.AdjustPaneSize({ "Down", 1 }) },
    { key = "LeftArrow", mods = mod.SUPER_REV, action = act.AdjustPaneSize({ "Left", 1 }) },
    { key = "RightArrow", mods = mod.SUPER_REV, action = act.AdjustPaneSize({ "Right", 1 }) },
  }
end

---Font-related bindings
local function font_bindings()
  return {
    -- fonts: resize
    { key = "UpArrow", mods = mod.SUPER, action = act.IncreaseFontSize },
    { key = "DownArrow", mods = mod.SUPER, action = act.DecreaseFontSize },
    { key = "r", mods = mod.SUPER, action = act.ResetFontSize },
  }
end

---Key table activation bindings
local function key_table_bindings()
  return {
    -- resizes fonts
    {
      key = "f",
      mods = "LEADER",
      action = act.ActivateKeyTable({
        name = "resize_font",
        one_shot = false,
        timemout_miliseconds = 1000,
      }),
    },
    -- resize panes
    {
      key = "p",
      mods = "LEADER",
      action = act.ActivateKeyTable({
        name = "resize_pane",
        one_shot = false,
        timemout_miliseconds = 1000,
      }),
    },
    -- rename tab bar
    {
      key = "R",
      mods = "CTRL|SHIFT",
      action = act.PromptInputLine({
        description = "Enter new name for tab",
        action = wezterm.action_callback(function(window, pane, line)
          -- line will be `nil` if they hit escape without entering anything
          -- An empty string if they just hit enter
          -- Or the actual line of text they wrote
          if line then
            window:active_tab():set_title(line)
          end
        end),
      }),
    },
  }
end

---Combine all keyboard bindings
local function combine_bindings()
  local all_bindings = {}
  local binding_groups = {
    misc_bindings(),
    copy_paste_bindings(),
    tab_bindings(),
    window_bindings(),
    pane_bindings(),
    font_bindings(),
    key_table_bindings(),
  }

  for _, group in ipairs(binding_groups) do
    for _, binding in ipairs(group) do
      table.insert(all_bindings, binding)
    end
  end

  return all_bindings
end

return {
  mod = mod,
  keys = combine_bindings(),
}

