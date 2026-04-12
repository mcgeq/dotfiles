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

return M
