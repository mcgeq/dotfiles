local M = {}

local common = require "lsp.servers.common"

function M.get()
  return {
    bashls = {
      name = "bashls",
      ensure_installed = true,
      config = {},
    },
    gopls = {
      name = "gopls",
      ensure_installed = require("user").private.features.go ~= false,
      enable = require("user").private.features.go ~= false,
      config = {
        root_markers = { "go.work", "go.mod", ".git" },
        settings = {
          gopls = {
            hints = {
              rangeVariableTypes = true,
              parameterNames = true,
              assignVariableTypes = true,
            },
          },
        },
      },
    },
    jsonls = {
      name = "jsonls",
      ensure_installed = true,
      config = {
        settings = {
          json = {
            schemas = common.schemastore "json",
            validate = { enable = true },
          },
        },
      },
    },
    lua_ls = {
      name = "lua_ls",
      ensure_installed = true,
      config = {
        settings = {
          Lua = {
            diagnostics = {
              globals = { "vim" },
            },
            hint = {
              enable = true,
            },
          },
        },
      },
    },
    marksman = {
      name = "marksman",
      ensure_installed = true,
      config = {},
    },
    rust_analyzer = {
      name = "rust_analyzer",
      ensure_installed = true,
      config = {
        root_markers = { "Cargo.toml", "rust-project.json", ".git" },
      },
    },
    taplo = {
      name = "taplo",
      ensure_installed = true,
      config = {},
    },
    yamlls = {
      name = "yamlls",
      ensure_installed = true,
      config = {
        settings = {
          yaml = {
            schemaStore = {
              enable = false,
              url = "",
            },
            schemas = common.schemastore "yaml",
            validate = true,
          },
        },
      },
    },
    zls = {
      name = "zls",
      ensure_installed = true,
      config = {
        root_markers = { "zls.json", "build.zig", ".git" },
      },
    },
  }
end

return M
