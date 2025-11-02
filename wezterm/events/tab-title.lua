---Tab title formatting event handler
---Customizes tab bar appearance with icons, colors, and indicators
---@see https://github.com/wez/wezterm/discussions/628#discussioncomment-1874614
local wezterm = require("wezterm")

local M = {}

---Constants
local GLYPH_SEMI_CIRCLE_LEFT = ""
local GLYPH_SEMI_CIRCLE_RIGHT = ""
local GLYPH_CIRCLE = "󰇷 "
local GLYPH_ADMIN = "󰖳 "

---Format items for tab title
M.cells = {}

---Color scheme for tab bar states
M.colors = {
  default = {
    bg = "#8C246F",
    fg = "#0F2536",
  },
  is_active = {
    bg = "#248C6E",
    fg = "#0F2536",
  },
  hover = {
    bg = "#786D22",
    fg = "#0F2536",
  },
  unseen_output = "#FF3B8B",
}

---Extract process name from full path
---@param path string Full path to process
---@return string Process name without path and extension
function M.set_process_name(path)
  if not path or type(path) ~= "string" then
    return ""
  end
  local name = string.gsub(path, "(.*[/\\])(.*)", "%2")
  return name:gsub("%.exe$", "")
end

---Format tab title with appropriate icon and text
---@param process_name string Process name
---@param static_title string Static tab title (if set)
---@param active_title string Active pane title
---@param max_width number Maximum width for title
---@param inset number? Inset/offset for title (default: 6)
---@return string Formatted title
function M.set_title(process_name, static_title, active_title, max_width, inset)
  inset = inset or 6
  local title = ""

  if process_name and process_name:len() > 0 and (not static_title or static_title:len() == 0) then
    title = "  " .. process_name .. " ~ "
  elseif static_title and static_title:len() > 0 then
    title = "󰌪  " .. static_title .. " ~ "
  else
    local safe_title = active_title and tostring(active_title) or ""
    title = "󰌽  " .. safe_title .. " ~ "
  end

  -- Truncate if too long
  if max_width and title:len() > max_width - inset then
    local diff = title:len() - max_width + inset
    title = wezterm.truncate_right(title, title:len() - diff)
  end

  return title
end

---Check if pane title indicates administrator/sudo session
---@param title string Pane title
---@return boolean True if admin session
function M.check_if_admin(title)
  if not title or type(title) ~= "string" then
    return false
  end
  return title:match("^Administrator: ") ~= nil
end

---Add a format item to the cells array
---@param bg string Background color
---@param fg string Foreground color
---@param attribute table Attribute (e.g., { Intensity = "Bold" })
---@param text string Text to display
function M.push(bg, fg, attribute, text)
  if not bg or not fg or not attribute or not text then
    wezterm.log_error("tab-title.push: Missing required parameters")
    return
  end

  table.insert(M.cells, { Background = { Color = bg } })
  table.insert(M.cells, { Foreground = { Color = fg } })
  table.insert(M.cells, { Attribute = attribute })
  table.insert(M.cells, { Text = text })
end

---Setup the tab title formatting event handler
function M.setup()
  wezterm.on("format-tab-title", function(tab, tabs, panes, config, hover, max_width)
    if not tab or not tab.active_pane then
      wezterm.log_error("tab-title.setup: Invalid tab or active_pane")
      return {}
    end

    -- Reset cells for this tab
    M.cells = {}

    -- Determine colors based on state
    local bg, fg
    if tab.is_active then
      bg = M.colors.is_active.bg
      fg = M.colors.is_active.fg
    elseif hover then
      bg = M.colors.hover.bg
      fg = M.colors.hover.fg
    else
      bg = M.colors.default.bg
      fg = M.colors.default.fg
    end

    -- Get process name and title
    local process_name = ""
    local active_title = ""
    local static_title = ""

    local ok = pcall(function()
      process_name = M.set_process_name(tab.active_pane.foreground_process_name or "")
      active_title = tab.active_pane.title or ""
      static_title = tab.tab_title or ""
    end)

    if not ok then
      wezterm.log_warn("tab-title.setup: Failed to get tab information")
    end

    local is_admin = M.check_if_admin(active_title)
    local title = M.set_title(process_name, static_title, active_title, max_width, (is_admin and 8) or 6)

    -- Check for unseen output
    local has_unseen_output = false
    if tab.panes then
      for _, pane in ipairs(tab.panes) do
        if pane.has_unseen_output then
          has_unseen_output = true
          break
        end
      end
    end

    -- Build the tab title
    -- Left semi-circle
    M.push(fg, bg, { Intensity = "Bold" }, GLYPH_SEMI_CIRCLE_LEFT)

    -- Admin Icon
    if is_admin then
      M.push(bg, fg, { Intensity = "Bold" }, " " .. GLYPH_ADMIN)
    end

    -- Title
    M.push(bg, fg, { Intensity = "Bold" }, " " .. title)

    -- Unseen output alert
    if has_unseen_output then
      M.push(bg, M.colors.unseen_output, { Intensity = "Bold" }, " " .. GLYPH_CIRCLE)
    end

    -- Right padding
    M.push(bg, fg, { Intensity = "Bold" }, " ")

    -- Right semi-circle
    M.push(fg, bg, { Intensity = "Bold" }, GLYPH_SEMI_CIRCLE_RIGHT)

    return M.cells
  end)
end

return M

-- local CMD_ICON = utf8.char(0xe62a)
-- local NU_ICON = utf8.char(0xe7a8)
-- local PS_ICON = utf8.char(0xe70f)
-- local ELV_ICON = utf8.char(0xfc6f)
-- local WSL_ICON = utf8.char(0xf83c)
-- local YORI_ICON = utf8.char(0xf1d4)
-- local NYA_ICON = utf8.char(0xf61a)
--
-- local VIM_ICON = utf8.char(0xe62b)
-- local PAGER_ICON = utf8.char(0xf718)
-- local FUZZY_ICON = utf8.char(0xf0b0)
-- local HOURGLASS_ICON = utf8.char(0xf252)
-- local SUNGLASS_ICON = utf8.char(0xf9df)
--
-- local PYTHON_ICON = utf8.char(0xf820)
-- local NODE_ICON = utf8.char(0xe74e)
-- local DENO_ICON = utf8.char(0xe628)
-- local LAMBDA_ICON = utf8.char(0xfb26)
--
-- local SOLID_LEFT_ARROW = utf8.char(0xe0ba)
-- local SOLID_LEFT_MOST = utf8.char(0x2588)
-- local SOLID_RIGHT_ARROW = utf8.char(0xe0bc)
-- local ADMIN_ICON = utf8.char(0xf49c)
