local M = {}

local common = require "lsp.servers.common"

local function merge(...)
  local merged = {}
  for _, table_to_merge in ipairs { ... } do
    merged = vim.tbl_deep_extend("force", merged, table_to_merge)
  end
  return merged
end

function M.get()
  local use_ty = common.python_ty_cmd() ~= nil
  local python = require "lsp.servers.python"
  local frontend = require "lsp.servers.frontend"
  local misc = require "lsp.servers.misc"

  return merge(python.get(use_ty), frontend.get(), misc.get())
end

return M
