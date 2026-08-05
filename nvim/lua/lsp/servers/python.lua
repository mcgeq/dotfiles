local M = {}

local common = require "lsp.servers.common"

function M.get(use_ty)
  local markers = common.PYTHON_ROOT_MARKERS

  return {
    basedpyright = {
      name = "basedpyright",
      ensure_installed = not use_ty,
      enable = not use_ty,
      config = {
        root_markers = markers,
        settings = {
          basedpyright = {
            analysis = {
              autoSearchPaths = true,
              typeCheckingMode = "standard",
              useLibraryCodeForTypes = true,
            },
          },
        },
      },
    },
    ruff = {
      name = "ruff",
      ensure_installed = true,
      config = {
        cmd = { "ruff", "server" },
        root_markers = markers,
        on_attach = function(client) client.server_capabilities.hoverProvider = false end,
      },
    },
    ty = {
      name = "ty",
      ensure_installed = false,
      enable = use_ty,
      config = {
        cmd = common.python_ty_cmd() or { "ty", "server" },
        filetypes = { "python" },
        root_markers = markers,
      },
    },
  }
end

return M
