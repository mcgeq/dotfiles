local M = {}

function M.directory(name)
  assert(type(name) == "string" and name ~= "." and name ~= ".." and not name:find "[/\\]", "Invalid plugin name")
  return vim.fs.joinpath(vim.fn.stdpath "data", "site", "pack", "core", "opt", name)
end

function M.status(path)
  if vim.fn.isdirectory(path) == 0 then return "missing" end
  local has_files = false
  for name in vim.fs.dir(path) do
    if name ~= ".git" then
      has_files = true
      break
    end
  end
  if not has_files then return "broken" end
  local ok, lines = pcall(vim.fn.readfile, vim.fs.joinpath(path, ".git", "HEAD"))
  if ok and lines[1] and lines[1]:find(".invalid", 1, true) then return "broken" end
  return "ok"
end

return M
