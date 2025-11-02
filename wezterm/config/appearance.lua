---Appearance configuration module
---Combines all appearance-related sub-modules
local appearance = {}

-- Import appearance sub-modules
local performance = require("config.appearance.performance")
local colors = require("config.appearance.colors")
local background = require("config.appearance.background")
local scrollbar = require("config.appearance.scrollbar")
local tab_bar = require("config.appearance.tab_bar")
local cursor = require("config.appearance.cursor")
local window = require("config.appearance.window")

-- Merge all appearance configurations
-- Note: Using Config:merge would be better, but we need to return a flat table here
-- since appearance.lua is used directly by the Config:append() method
for k, v in pairs(performance) do
  appearance[k] = v
end

for k, v in pairs(colors) do
  appearance[k] = v
end

for k, v in pairs(background) do
  appearance[k] = v
end

-- Special handling for nested colors table (scrollbar_thumb)
if scrollbar.colors then
  if not appearance.colors then
    appearance.colors = {}
  end
  for k, v in pairs(scrollbar.colors) do
    appearance.colors[k] = v
  end
end
for k, v in pairs(scrollbar) do
  if k ~= "colors" then
    appearance[k] = v
  end
end

for k, v in pairs(tab_bar) do
  appearance[k] = v
end

for k, v in pairs(cursor) do
  appearance[k] = v
end

for k, v in pairs(window) do
  appearance[k] = v
end

return appearance
