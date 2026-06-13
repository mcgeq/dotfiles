local M = {}

local loaded = {}

function M.ensure(name)
  if loaded[name] then return true end

  local ok, err = pcall(vim.cmd.packadd, name)
  if not ok then
    vim.notify("Failed to load plugin `" .. name .. "`: " .. tostring(err), vim.log.levels.ERROR, {
      title = "packadd",
    })
    return false
  end

  loaded[name] = true
  return true
end

function M.ensure_many(names)
  for _, name in ipairs(names or {}) do
    if not M.ensure(name) then return false end
  end
  return true
end

return M
