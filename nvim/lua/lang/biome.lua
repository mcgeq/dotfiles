local M = {}

local util = require "core.util"

local BIOME_CONFIG_MARKERS = { "biome.json", "biome.jsonc" }

local function project_root(bufnr) return vim.fs.root(bufnr, BIOME_CONFIG_MARKERS) end

--- Whether the project this buffer belongs to uses Biome.
--- @param bufnr integer
--- @return string? root Biome config root, or nil if not a Biome project
function M.project_root(bufnr) return project_root(bufnr) end

local function local_biome(root)
  local candidates = {
    vim.fs.joinpath(root, "node_modules", ".bin", "biome" .. util.executable_suffix()),
    vim.fs.joinpath(root, "node_modules", "@biomejs", "biome", "bin", "biome"),
  }
  for _, candidate in ipairs(candidates) do
    if vim.fn.executable(candidate) == 1 then return candidate end
  end
end

local function mason_biome()
  local package = vim.fs.joinpath(vim.fn.stdpath "data", "mason", "packages", "biome")
  local candidates = {
    vim.fs.joinpath(package, "biome" .. util.executable_suffix()),
    vim.fs.joinpath(package, "bin", "biome" .. util.executable_suffix()),
  }
  for _, candidate in ipairs(candidates) do
    if vim.fn.executable(candidate) == 1 then return candidate end
  end
end

--- Locate the Biome binary: project-local, then PATH, then mason.
--- @param bufnr integer
--- @return string? path
function M.binary(bufnr)
  local root = project_root(bufnr)
  local local_bin = root and local_biome(root)
  if local_bin then return local_bin end
  if vim.fn.executable "biome" == 1 then return "biome" end
  return mason_biome()
end

--- LSP command for Biome. Returns nil when no binary can be found; the
--- server will then simply not attach.
--- @param bufnr integer
--- @return string[]?
function M.lsp_cmd(bufnr)
  local binary = M.binary(bufnr)
  if not binary then return nil end
  return { binary, "lsp-proxy" }
end

function M.supports(bufnr)
  return vim.list_contains(
    { "javascript", "javascriptreact", "typescript", "typescriptreact", "vue", "json", "jsonc" },
    vim.bo[bufnr].filetype
  )
end

function M.lsp_root_dir(bufnr, cb)
  local root = project_root(bufnr)
  if root and M.binary(bufnr) then cb(root) end
end

function M.lsp_cmd_fn(dispatchers, config)
  local binary = M.binary(config.root_dir)
  assert(binary, "Biome binary disappeared for " .. tostring(config.root_dir))
  return vim.lsp.rpc.start({ binary, "lsp-proxy" }, dispatchers, { cwd = config.root_dir })
end

function M.apply_code_action(only, bufnr)
  return require("lsp.actions").apply(bufnr or vim.api.nvim_get_current_buf(), only, { "biome" })
end

--- Fix all lint issues with Biome.
--- @return boolean applied
function M.fix_all(bufnr) return M.apply_code_action("source.fixAll.biome", bufnr) end

--- Organize imports with Biome.
--- @return boolean applied
function M.organize_imports(bufnr) return M.apply_code_action("source.organizeImports.biome", bufnr) end

--- Whether the current buffer has an attached Biome LSP client.
--- @param bufnr integer
--- @return boolean
function M.has_client(bufnr) return #vim.lsp.get_clients { bufnr = bufnr, name = "biome" } > 0 end

--- Format the buffer through the Biome LSP client. `source.fixAll.biome`
--- only applies lint fixes, so formatting needs its own request.
--- @param bufnr integer
function M.format(bufnr, opts)
  if not M.has_client(bufnr) then return end
  vim.lsp.buf.format(vim.tbl_extend("force", { bufnr = bufnr, name = "biome", timeout_ms = 1000 }, opts or {}))
end

return M
