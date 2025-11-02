local wezterm = require("wezterm")

---@class Config
---@field options table
local Config = {}

---Check if a table is an array (list)
---@param t table
---@return boolean
local function is_array(t)
  if type(t) ~= "table" then
    return false
  end
  local count = 0
  for _ in pairs(t) do
    count = count + 1
  end
  -- Check if all keys are consecutive integers starting from 1
  for i = 1, count do
    if t[i] == nil then
      return false
    end
  end
  return count > 0
end

---Deep merge two tables
---@param target table
---@param source table
---@return table
local function deep_merge(target, source)
  for k, v in pairs(source) do
    if type(v) == "table" and type(target[k]) == "table" and not is_array(v) then
      -- Recursively merge tables (but not arrays)
      target[k] = deep_merge(target[k] or {}, v)
    else
      -- Overwrite or assign
      target[k] = v
    end
  end
  return target
end

---Initialize Config
---@return Config
function Config:init()
  local o = {}
  self = setmetatable(o, { __index = Config })
  self.options = {}
  return o
end

---Append to `Config.options` (shallow merge)
---@param new_options table new options to append
---@return Config
function Config:append(new_options)
  if type(new_options) ~= "table" then
    wezterm.log_error("Config:append expects a table, got " .. type(new_options))
    return self
  end

  for k, v in pairs(new_options) do
    if self.options[k] ~= nil then
      wezterm.log_warn(
        string.format('Duplicate config option detected: "%s"', k),
        { old = self.options[k], new = new_options[k] }
      )
      goto continue
    end
    self.options[k] = v
    ::continue::
  end
  return self
end

---Deep merge into `Config.options`
---Useful for merging nested configurations like colors, window_frame, etc.
---@param new_options table new options to merge
---@return Config
function Config:merge(new_options)
  if type(new_options) ~= "table" then
    wezterm.log_error("Config:merge expects a table, got " .. type(new_options))
    return self
  end

  self.options = deep_merge(self.options, new_options)
  return self
end

---Get a copy of current options (for debugging)
---@return table
function Config:get_options()
  local wezterm = require("wezterm")
  -- Create a deep copy
  local function deep_copy(obj)
    if type(obj) ~= "table" then
      return obj
    end
    if wezterm.GLOBAL then
      -- Use wezterm's built-in deep copy if available
      return wezterm.json_parse(wezterm.json_encode(obj))
    end
    -- Fallback: manual deep copy
    local copy = {}
    for k, v in pairs(obj) do
      copy[k] = deep_copy(v)
    end
    return copy
  end
  return deep_copy(self.options)
end

return Config
