local M = {}

local common = require "lsp.servers.common"

local function get_vue_plugin()
  local location = vim.fs.joinpath(
    vim.fn.stdpath "data",
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
  local markers = common.FRONTEND_ROOT_MARKERS
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
    biome = {
      name = "biome",
      ensure_installed = false, -- 项目级工具：node_modules / PATH / mason 探测
      config = {
        cmd = function(dispatchers)
          local biome = require "lang.biome"
          return biome.lsp_cmd_fn(dispatchers)
        end,
        root_dir = function(bufnr, cb) require("lang.biome").lsp_root_dir(bufnr, cb) end,
        single_file_support = false,
        filetypes = {
          "javascript",
          "javascriptreact",
          "typescript",
          "typescriptreact",
          "vue",
          "json",
          "jsonc",
        },
        settings = {
          formatter = {
            enabled = true,
          },
        },
      },
    },
    cssls = {
      name = "cssls",
      ensure_installed = true,
      config = {
        root_markers = markers,
      },
    },
    eslint = {
      name = "eslint",
      ensure_installed = true,
      config = {
        cmd = { "vscode-eslint-language-server", "--stdio" },
        root_markers = markers,
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
        root_markers = markers,
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
    vtsls = {
      name = "vtsls",
      ensure_installed = true,
      config = {
        cmd = { "vtsls", "--stdio" },
        root_markers = markers,
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
        root_markers = markers,
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
  }
end

return M
