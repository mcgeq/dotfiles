local M = {}

local FRONTEND_ROOT_MARKERS = {
  "tsconfig.json",
  "jsconfig.json",
  "package.json",
  "eslint.config.js",
  "eslint.config.mjs",
  "eslint.config.cjs",
  "eslint.config.ts",
  "biome.json",
  "biome.jsonc",
  "deno.json",
  "deno.jsonc",
  "bun.lockb",
  "pnpm-workspace.yaml",
  ".git",
}

local PYTHON_ROOT_MARKERS = {
  "pyproject.toml",
  "uv.lock",
  "ruff.toml",
  ".ruff.toml",
  "requirements.txt",
  ".git",
}

M.FRONTEND_ROOT_MARKERS = FRONTEND_ROOT_MARKERS
M.PYTHON_ROOT_MARKERS = PYTHON_ROOT_MARKERS

function M.schemastore(kind)
  local ok, store = pcall(require, "schemastore")
  if not ok or not store[kind] or not store[kind].schemas then return nil end
  return store[kind].schemas()
end

return M
