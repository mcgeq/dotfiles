--- @param config FrontendConfig
return function(config)
  return {
    getFormatOnSavePattern = function()
      if config.format_on_save then
        return { "*.ts", "*.tsx", "*.js", "*.jsx" }
      end
      return {}
    end,

    getTSEnsureList = function()
      local list = { "html", "css", "javascript", "typescript", "tsx" }
      if config.prisma then
        table.insert(list, "prisma")
      end
      if config.vue then
        table.insert(list, "vue")
      end
      return list
    end,

    getLSPEnsureList = function()
      local list = { "ts_ls", "cssls", "emmet_ls", "html" }
      if config.tailwindcss then
        table.insert(list, "tailwindcss")
      end
      if config.prisma then
        table.insert(list, "prismals")
      end
      if config.vue then
        table.insert(list, "volar")
      end
      if config.linter == "eslint" then
        table.insert(list, "eslint")
      end
      return list
    end,

    getLSPConfigMap = function()
      local list = {
        -- tsserver config is moved to pisces/plugins/typescript-tools.lua
        -- ts_ls = require("pisces.lsp.config.typescript"),
        cssls = require("pisces.lsp.config.css"),
        emmet_ls = require("pisces.lsp.config.emmet"),
        html = require("pisces.lsp.config.html"),
      }
      if config.linter == "eslint" then
        list.eslint = require("pisces.lsp.config.eslint")
      end
      if config.tailwindcss then
        list.tailwindcss = require("pisces.lsp.config.tailwindcss")
      end
      if config.vue then
        list.volar = require("pisces.lsp.config.volar")
      end
      if config.prisma then
        list.prismals = require("pisces.lsp.config.prismals")
      end

      return list
    end,

    getToolEnsureList = function()
      local list = {}
      if config.formatter == "prettier" then
        table.insert(list, "prettier")
      end
      if config.cspell then
        table.insert(list, "cspell")
      end
      return list
    end,

    getNulllsSources = function()
      local null_ls = pRequire("null-ls")
      if not null_ls then
        return {}
      end
      local list = {}
      if config.formatter == "prettier" then
        table.insert(
          list,
          null_ls.builtins.formatting.prettier.with({
            filetypes = {
              "javascript",
              "javascriptreact",
              "typescript",
              "typescriptreact",
              "vue",
              "css",
              "scss",
              "less",
              "html",
              "graphql",
              -- "json",
              -- "yaml",
              -- "markdown",
            },
            timeout = 10000,
            prefer_local = "node_modules/.bin",
          })
        )
      end
      if config.cspell then
        table.insert(
          list,
          null_ls.builtins.diagnostics.cspell.with({
            filetypes = {
              "javascript",
              "javascriptreact",
              "typescript",
              "typescriptreact",
            },
          })
        )
        table.insert(
          list,
          null_ls.builtins.code_actions.cspell.with({
            "javascript",
            "javascriptreact",
            "typescript",
            "typescriptreact",
          })
        )
      end
      return list
    end,

    getNeotestAdapters = function()
      return {}
    end,
  }
end
