local M = {}

---@param module string
---@return any?
function M.require_if_exists(module)
  local ok, value = pcall(require, module)
  if ok then return value end
end

---@param root string
---@return string[]
function M.scan_modules(root)
  local modules = {}
  local root_path = vim.fn.stdpath("config") .. "/lua/" .. root:gsub("%.", "/")
  if vim.fn.isdirectory(root_path) == 0 then return modules end

  for name, type_ in vim.fs.dir(root_path) do
    if type_ == "file" and name:sub(-4) == ".lua" and name ~= "init.lua" then
      table.insert(modules, root .. "." .. name:gsub("%.lua$", ""))
    end
  end

  table.sort(modules)
  return modules
end

---@param root string
---@return table[]
function M.load_table_modules(root)
  local loaded = {}
  for _, module in ipairs(M.scan_modules(root)) do
    local ok, value = pcall(require, module)
    if ok and value ~= nil then table.insert(loaded, value) end
  end
  return loaded
end

---@param names string[]|nil
---@return table<string, boolean>
function M.to_lookup(names)
  local lookup = {}
  for _, name in ipairs(names or {}) do
    lookup[name] = true
  end
  return lookup
end

function M.is_windows()
  return vim.fn.has("win32") == 1 or vim.fn.has("win64") == 1
end

function M.executable_suffix()
  return M.is_windows() and ".exe" or ""
end

function M.first_executable(names)
  for _, name in ipairs(names) do
    local path = vim.fn.exepath(name)
    if path and path ~= "" then return path end
  end
end

function M.powershell_quote(value)
  return "'" .. tostring(value):gsub("'", "''") .. "'"
end

function M.shell_quote(value)
  if M.is_windows() then return M.powershell_quote(value) end
  return "'" .. tostring(value):gsub("'", [['"'"']]) .. "'"
end

return M
