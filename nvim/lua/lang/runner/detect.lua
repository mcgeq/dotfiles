local M = {}

local util = require "core.util"
local is_windows = util.is_windows

function M.file_exists(path) return path and vim.uv.fs_stat(path) ~= nil end

function M.is_rust_project(bufnr) return vim.fs.root(bufnr, { "Cargo.toml", "rust-project.json", ".git" }) end

function M.is_go_project(bufnr) return vim.fs.root(bufnr, { "go.work", "go.mod", ".git" }) end

function M.is_zig_project(bufnr) return vim.fs.root(bufnr, { "build.zig", "zls.json", ".git" }) end

function M.is_python_project(bufnr)
  return vim.fs.root(bufnr, { "pyproject.toml", "uv.lock", "requirements.txt", ".git" })
end

function M.package_json_root(bufnr) return vim.fs.root(bufnr, { "package.json" }) end

function M.find_upward(start_dir, names)
  if not start_dir or start_dir == "" then return end
  local dir = vim.fs.normalize(start_dir)
  while dir and dir ~= "" do
    for _, name in ipairs(names) do
      local path = vim.fs.joinpath(dir, name)
      if M.file_exists(path) then return path, dir end
    end
    local parent = vim.fs.dirname(dir)
    if not parent or parent == dir then break end
    dir = parent
  end
end

function M.find_local_python(start_dir)
  local path = select(
    1,
    M.find_upward(start_dir, {
      is_windows() and ".venv\\Scripts\\python.exe" or ".venv/bin/python",
      is_windows() and "venv\\Scripts\\python.exe" or "venv/bin/python",
    })
  )
  return path
end

function M.package_json_data(root)
  if not root then return end
  local path = vim.fs.joinpath(root, "package.json")
  if not M.file_exists(path) then return end

  local ok_read, lines = pcall(vim.fn.readfile, path)
  if not ok_read then return end

  local ok_decode, data = pcall(vim.json.decode, table.concat(lines, "\n"))
  if ok_decode and type(data) == "table" then return data end
end

function M.detect_package_manager(root)
  local lock_path = select(
    1,
    M.find_upward(root, {
      "pnpm-lock.yaml",
      "bun.lockb",
      "bun.lock",
      "yarn.lock",
      "package-lock.json",
    })
  )

  if lock_path then
    local name = vim.fs.basename(lock_path)
    if name == "pnpm-lock.yaml" and vim.fn.executable "pnpm" == 1 then
      return "pnpm"
    elseif (name == "bun.lockb" or name == "bun.lock") and vim.fn.executable "bun" == 1 then
      return "bun"
    elseif name == "yarn.lock" and vim.fn.executable "yarn" == 1 then
      return "yarn"
    elseif name == "package-lock.json" and vim.fn.executable "npm" == 1 then
      return "npm"
    end
  end

  if vim.fn.executable "pnpm" == 1 then
    return "pnpm"
  elseif vim.fn.executable "bun" == 1 then
    return "bun"
  elseif vim.fn.executable "yarn" == 1 then
    return "yarn"
  elseif vim.fn.executable "npm" == 1 then
    return "npm"
  end
end

function M.package_manager_command(manager, script)
  if manager == "bun" then
    return "bun run " .. script
  elseif manager == "yarn" then
    return "yarn run " .. script
  elseif manager == "pnpm" then
    return "pnpm run " .. script
  end
  return "npm run " .. script
end

return M
