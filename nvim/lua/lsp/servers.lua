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

local function has_executable(name)
  return vim.fn.executable(name) == 1
end

local function python_ty_cmd()
  if has_executable("ty") then return { "ty", "server" } end
  if has_executable("uv") and vim.env.VIRTUAL_ENV and vim.env.VIRTUAL_ENV ~= "" then
    return { "uv", "run", "ty", "server" }
  end
  return nil
end

local function get_vue_plugin()
  local location = vim.fs.joinpath(
    vim.fn.stdpath("data"),
    "mason",
    "packages",
    "vue-language-server",
    "node_modules",
    "@vue",
    "language-server"
  )

  if vim.uv.fs_stat(location) then
    return {
      name = "@vue/typescript-plugin",
      location = location,
      languages = { "vue" },
      configNamespace = "typescript",
    }
  end
end

local function schemastore(kind)
  local ok, store = pcall(require, "schemastore")
  if not ok or not store[kind] or not store[kind].schemas then return nil end
  return store[kind].schemas()
end

local function ts_on_attach(client, bufnr)
  local filetype = vim.bo[bufnr].filetype
  local semantic = client.server_capabilities.semanticTokensProvider
  if semantic then semantic.full = filetype ~= "vue" end
end

local function vue_on_init(client)
  local unpack_fn = table.unpack

  client.handlers["tsserver/request"] = function(_, result, context)
    local ts_client = vim.lsp.get_clients({ bufnr = context.bufnr, name = "vtsls" })[1]
    if not ts_client then
      vim.notify("Could not find `vtsls` client, `vue_ls` TS bridge is unavailable.", vim.log.levels.WARN, {
        title = "Vue LSP",
      })
      return
    end

    local params = result and result[1]
    if not params then return end

    local id, command, payload = unpack_fn(params)
    ts_client:exec_cmd({
      title = "vue_request_forward",
      command = "typescript.tsserverRequest",
      arguments = { command, payload },
    }, { bufnr = context.bufnr }, function(_, response)
      local body = response and response.body
      ---@diagnostic disable-next-line: param-type-mismatch
      client:notify("tsserver/response", { { id, body } })
    end)
  end
end

function M.get()
  local vue_plugin = get_vue_plugin()
  local ty_cmd = python_ty_cmd()
  local use_ty = ty_cmd ~= nil
  local ts_filetypes = { "typescript", "javascript", "javascriptreact", "typescriptreact", "vue" }
  local emmet_filetypes = {
    "css",
    "eruby",
    "html",
    "javascriptreact",
    "less",
    "pug",
    "sass",
    "scss",
    "typescriptreact",
    "vue",
  }

  return {
    bashls = {
      name = "bashls",
      ensure_installed = true,
      config = {},
    },
    basedpyright = {
      name = "basedpyright",
      ensure_installed = not use_ty,
      enable = not use_ty,
      config = {
        root_markers = PYTHON_ROOT_MARKERS,
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
    cssls = {
      name = "cssls",
      ensure_installed = true,
      config = {
        root_markers = FRONTEND_ROOT_MARKERS,
      },
    },
    eslint = {
      name = "eslint",
      ensure_installed = true,
      config = {
        cmd = { "vscode-eslint-language-server", "--stdio" },
        root_markers = FRONTEND_ROOT_MARKERS,
        single_file_support = false,
        filetypes = {
          "css",
          "html",
          "javascript",
          "javascriptreact",
          "json",
          "jsonc",
          "less",
          "markdown",
          "scss",
          "toml",
          "typescript",
          "typescriptreact",
          "vue",
          "yaml",
        },
        settings = {
          validate = "on",
          packageManager = "npm",
          useESLintClass = false,
          experimental = {
            useFlatConfig = false,
          },
          codeActionOnSave = {
            enable = false,
            mode = "all",
          },
          format = false,
          quiet = false,
          onIgnoredFiles = "off",
          run = "onType",
          problems = {
            shortenToSingleLine = false,
          },
          nodePath = vim.NIL,
          workingDirectory = {
            mode = "auto",
          },
          codeAction = {
            disableRuleComment = {
              enable = true,
              location = "separateLine",
            },
            showDocumentation = {
              enable = true,
            },
          },
          rulesCustomizations = {
            { rule = "style/*", severity = "off", fixable = true },
            { rule = "format/*", severity = "off", fixable = true },
            { rule = "*-indent", severity = "off", fixable = true },
            { rule = "*-spacing", severity = "off", fixable = true },
            { rule = "*-spaces", severity = "off", fixable = true },
            { rule = "*-order", severity = "off", fixable = true },
            { rule = "*-dangle", severity = "off", fixable = true },
            { rule = "*-newline", severity = "off", fixable = true },
            { rule = "*quotes", severity = "off", fixable = true },
            { rule = "*semi", severity = "off", fixable = true },
          },
        },
      },
    },
    emmet_language_server = {
      name = "emmet_language_server",
      ensure_installed = true,
      config = {
        root_markers = FRONTEND_ROOT_MARKERS,
        filetypes = emmet_filetypes,
        init_options = {
          includeLanguages = {
            vue = "html",
          },
          showAbbreviationSuggestions = true,
          showExpandedAbbreviation = "always",
          showSuggestionsAsSnippets = false,
        },
      },
    },
    gopls = {
      name = "gopls",
      ensure_installed = true,
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
            schemas = schemastore("json"),
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
    ruff = {
      name = "ruff",
      ensure_installed = true,
      config = {
        cmd = { "ruff", "server" },
        root_markers = PYTHON_ROOT_MARKERS,
        on_attach = function(client)
          client.server_capabilities.hoverProvider = false
        end,
      },
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
    vtsls = {
      name = "vtsls",
      ensure_installed = true,
      config = {
        cmd = { "vtsls", "--stdio" },
        root_markers = FRONTEND_ROOT_MARKERS,
        single_file_support = false,
        filetypes = ts_filetypes,
        on_attach = ts_on_attach,
        settings = {
          vtsls = vue_plugin and {
            tsserver = {
              globalPlugins = { vue_plugin },
            },
          } or {},
          typescript = {
            inlayHints = {
              includeInlayParameterNameHints = "literal",
              includeInlayParameterNameHintsWhenArgumentMatchesName = false,
              includeInlayFunctionParameterTypeHints = true,
              includeInlayVariableTypeHints = false,
              includeInlayPropertyDeclarationTypeHints = true,
              includeInlayFunctionLikeReturnTypeHints = true,
              includeInlayEnumMemberValueHints = true,
            },
            preferences = {
              includePackageJsonAutoImports = "auto",
              importModuleSpecifier = "non-relative",
              jsxAttributeCompletionStyle = "auto",
            },
            suggest = {
              completeFunctionCalls = true,
            },
          },
          javascript = {
            inlayHints = {
              includeInlayParameterNameHints = "literal",
              includeInlayParameterNameHintsWhenArgumentMatchesName = false,
              includeInlayFunctionParameterTypeHints = true,
              includeInlayVariableTypeHints = false,
              includeInlayPropertyDeclarationTypeHints = true,
              includeInlayFunctionLikeReturnTypeHints = true,
              includeInlayEnumMemberValueHints = true,
            },
            preferences = {
              includePackageJsonAutoImports = "auto",
              importModuleSpecifier = "non-relative",
              jsxAttributeCompletionStyle = "auto",
            },
            suggest = {
              completeFunctionCalls = true,
            },
          },
        },
      },
    },
    vue_ls = {
      name = "vue_ls",
      ensure_installed = true,
      config = {
        cmd = { "vue-language-server", "--stdio" },
        root_markers = FRONTEND_ROOT_MARKERS,
        single_file_support = false,
        filetypes = { "vue" },
        on_init = vue_on_init,
        settings = {
          vue = {
            inlayHints = {
              missingProps = true,
              inlineHandlerLeading = true,
              optionsWrapper = true,
              vBindShorthand = true,
            },
          },
        },
      },
    },
    ty = {
      name = "ty",
      ensure_installed = false,
      enable = use_ty,
      config = {
        cmd = ty_cmd or { "ty", "server" },
        filetypes = { "python" },
        root_markers = PYTHON_ROOT_MARKERS,
      },
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
            schemas = schemastore("yaml"),
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
