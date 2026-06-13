local function split(str, delimiter)
  local result = {}
  local from = 1
  local delim_from, delim_to = string.find(str, delimiter, from)
  while delim_from do
    table.insert(result, string.sub(str, from, delim_from - 1))
    from = delim_to + 1
    delim_from, delim_to = string.find(str, delimiter, from)
  end
  table.insert(result, string.sub(str, from))
  return result
end

local function is_shell_available(shell)
  local env_path = os.getenv("PATH")
  if not env_path then
    return false
  end

  local sep = package.config:sub(1, 1) == "\\" and ";" or ":"
  local paths = split(env_path, sep)

  for _, path in ipairs(paths) do
    local full_path = path .. "/" .. shell
    if package.config:sub(1, 1) == "\\" then
      full_path = full_path .. ".exe"
    end
    local f = io.open(full_path, "r")
    if f then
      f:close()
      return true
    end
  end
  return false
end

local function get_shell_available(shells)
  if #shells == 0 then
    return "None"
  end

  for _, shell in ipairs(shells) do
    if is_shell_available(shell) then
      return shell
    end
  end
  return "None"
end

local shells = { "pwsh", "powershell", "bash", "nu", "fish", "zsh" }
local default_shell = get_shell_available(shells)

return default_shell
