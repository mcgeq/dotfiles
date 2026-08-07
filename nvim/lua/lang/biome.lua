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

--- Cache for the most recently resolved binary, set by lsp_root_dir.
M._last_binary = nil

--- Dynamic LSP root_dir: only activates Biome when the project uses it and a
--- binary is available. `cb` is not called otherwise, so LSP never starts.
--- @param bufnr integer
--- @param cb fun(root_dir: string)
function M.lsp_root_dir(bufnr, cb)
  local root = project_root(bufnr)
  if not root then return end
  local binary = M.binary(bufnr)
  if not binary then return end
  M._last_binary = binary
  cb(root)
end

--- Dynamic LSP cmd, called after lsp_root_dir succeeded. Resolves the binary
--- from the cache set during root detection.
--- @param dispatchers vim.lsp.rpc.Dispatchers
--- @return vim.lsp.rpc.Client
function M.lsp_cmd_fn(dispatchers)
  local binary = M._last_binary or "biome"
  return vim.lsp.rpc.start({ binary, "lsp-proxy" }, dispatchers, {})
end
--- Run a Biome code action synchronously on the current buffer.
--- @param only string
--- @return boolean applied
function M.apply_code_action(only)
  local bufnr = vim.api.nvim_get_current_buf()
  local params = vim.lsp.util.make_range_params(nil, "utf-8")
  params.context = {
    only = { only },
    diagnostics = vim.diagnostic.get(bufnr),
    triggerKind = vim.lsp.protocol.CodeActionTriggerKind.Automatic,
  }

  for _, client in ipairs(vim.lsp.get_clients { bufnr = bufnr, name = "biome" }) do
    local response = client:request_sync("textDocument/codeAction", params, 1200, bufnr)
    local actions = response and response.result or nil
    if actions and not vim.tbl_isempty(actions) then
      for _, action in ipairs(actions) do
        if action.edit then vim.lsp.util.apply_workspace_edit(action.edit, client.offset_encoding or "utf-8") end
        if action.command then
          local command = type(action.command) == "table" and action.command or action
          pcall(vim.lsp.buf.execute_command, command)
        end
      end
      return true
    end
  end

  return false
end

--- Fix all lint issues with Biome.
--- @return boolean applied
function M.fix_all() return M.apply_code_action "source.fixAll.biome" end

--- Organize imports with Biome.
--- @return boolean applied
function M.organize_imports() return M.apply_code_action "source.organizeImports.biome" end

--- Whether the current buffer has an attached Biome LSP client.
--- @param bufnr integer
--- @return boolean
function M.has_client(bufnr) return #vim.lsp.get_clients { bufnr = bufnr, name = "biome" } > 0 end

--- Format the buffer through the Biome LSP client. `source.fixAll.biome`
--- only applies lint fixes, so formatting needs its own request.
--- @param bufnr integer
function M.format(bufnr)
  if not M.has_client(bufnr) then return end
  vim.lsp.buf.format { bufnr = bufnr, name = "biome" }
end

return M
