local function merge(target, source)
  for k, v in pairs(source) do
    if type(v) == "table" and type(target[k]) == "table" then
      for k2, v2 in pairs(v) do
        target[k][k2] = v2
      end
    else
      target[k] = v
    end
  end
end

local performance = require("config.appearance.performance")
local colors = require("config.appearance.colors")
local background = require("config.appearance.background")
local scrollbar = require("config.appearance.scrollbar")
local tab_bar = require("config.appearance.tab_bar")
local cursor = require("config.appearance.cursor")
local window = require("config.appearance.window")

local appearance = {}
merge(appearance, performance)
merge(appearance, colors)
merge(appearance, background)
merge(appearance, scrollbar)
merge(appearance, tab_bar)
merge(appearance, cursor)
merge(appearance, window)

return appearance
