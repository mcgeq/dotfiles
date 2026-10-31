local M = {}

local common = require "lsp.servers.common"

function M.get()
  local markers = common.PYTHON_ROOT_MARKERS

  return {
    basedpyright = {
      name = "basedpyright",
      ensure_installed = true,
      install_for = function(bufnr)
        return require("lang.python").checker(vim.api.nvim_buf_get_name(bufnr)) == "basedpyright"
      end,
      config = {
        cmd = require("lang.python").lsp_command { "basedpyright-langserver", "--stdio" },
        before_init = require("lang.python").before_init,
        root_dir = require("lang.python").checker_root "basedpyright",
        filetypes = { "python" },
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
        cmd = require("lang.python").lsp_command { "ruff", "server" },
        root_markers = markers,
        on_attach = function(client) client.server_capabilities.hoverProvider = false end,
      },
    },
    ty = {
      name = "ty",
      ensure_installed = false,
      config = {
        cmd = function(dispatchers, config)
          local command =
            assert(require("lang.python").ty_command(config.root_dir), "Ty is not available for this project")
          return require("lang.python").lsp_command(command)(dispatchers, config)
        end,
        before_init = require("lang.python").before_init,
        root_dir = require("lang.python").checker_root "ty",
        filetypes = { "python" },
        settings = {},
        root_markers = markers,
      },
    },
  }
end

return M
