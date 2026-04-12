local M = {}

function M.setup()
  local map = require("core.keymaps").map
  local terminal = require("core.terminal")
  local frontend_state = require("lang.frontend")
  local frontend_clients = { "eslint", "vtsls", "vue_ls" }

  local function has_client(bufnr, names)
    local lookup = {}
    for _, name in ipairs(names or {}) do
      lookup[name] = true
    end
    for _, client in ipairs(vim.lsp.get_clients({ bufnr = bufnr })) do
      if lookup[client.name] then return true end
    end
    return false
  end

  local function apply_sync_code_action(bufnr, only, client_names)
    local allowed = {}
    for _, name in ipairs(client_names or {}) do
      allowed[name] = true
    end

    local params = vim.lsp.util.make_range_params(nil, "utf-8")
    params.context = {
      only = { only },
      diagnostics = vim.diagnostic.get(bufnr),
      triggerKind = vim.lsp.protocol.CodeActionTriggerKind.Automatic,
    }

    for _, client in ipairs(vim.lsp.get_clients({ bufnr = bufnr })) do
      if not client_names or allowed[client.name] then
        local response = client:request_sync("textDocument/codeAction", params, 1200, bufnr)
        local actions = response and response.result or nil
        if actions and not vim.tbl_isempty(actions) then
          for _, action in ipairs(actions) do
            if action.edit then
              vim.lsp.util.apply_workspace_edit(action.edit, client.offset_encoding or "utf-8")
            end
            if action.command then
              local command = type(action.command) == "table" and action.command or action
              pcall(vim.lsp.buf.execute_command, command)
            end
          end
          return true
        end
      end
    end

    return false
  end

  local function preferred_code_action(only, client_names)
    return function()
      local bufnr = vim.api.nvim_get_current_buf()

      if only == "source.fixAll.eslint" and vim.fn.exists(":EslintFixAll") == 2 and has_client(bufnr, { "eslint" }) then
        pcall(vim.cmd, "silent EslintFixAll")
        return
      end

      if apply_sync_code_action(bufnr, only, client_names) then return end

      vim.lsp.buf.code_action({
        apply = true,
        context = {
          only = { only },
          diagnostics = vim.diagnostic.get(bufnr),
        },
      })
    end
  end

  local function show_frontend_clients(bufnr)
    local names = {}
    for _, client in ipairs(vim.lsp.get_clients({ bufnr = bufnr })) do
      if vim.list_contains(frontend_clients, client.name) then
        table.insert(names, client.name)
      end
    end
    table.sort(names)

    local message = #names > 0 and table.concat(names, ", ") or "none"
    vim.notify("Frontend LSP: " .. message, vim.log.levels.INFO, { title = "Frontend Clients" })
  end

  vim.api.nvim_create_autocmd("ColorScheme", {
    group = vim.api.nvim_create_augroup("plugins_frontend_vue_highlights", { clear = true }),
    callback = function()
      vim.api.nvim_set_hl(0, "@lsp.type.component", { link = "@type" })
    end,
  })
  vim.api.nvim_set_hl(0, "@lsp.type.component", { link = "@type" })

  local ok_colorizer, colorizer = pcall(require, "colorizer")
  if ok_colorizer then
    colorizer.setup({
      filetypes = {
        "css",
        "scss",
        "sass",
        "less",
        "html",
        "javascript",
        "typescript",
        "javascriptreact",
        "typescriptreact",
        "vue",
        "svelte",
      },
      user_default_options = {
        RGB = true,
        RRGGBB = true,
        names = true,
        RRGGBBAA = true,
        AARRGGBB = true,
        rgb_fn = true,
        hsl_fn = true,
        css = true,
        css_fn = true,
        mode = "background",
        tailwind = true,
      },
    })
  end

  local ok_autotag, autotag = pcall(require, "nvim-ts-autotag")
  if ok_autotag then
    autotag.setup({
      opts = {
        enable_close = true,
        enable_rename = true,
        enable_close_on_slash = false,
      },
    })
  end

  vim.api.nvim_create_autocmd("FileType", {
    group = vim.api.nvim_create_augroup("plugins_frontend_lsp_keymaps", { clear = true }),
    pattern = { "javascript", "javascriptreact", "typescript", "typescriptreact", "vue" },
    callback = function(event)
      map("n", "<localleader>lf", preferred_code_action("source.fixAll.eslint", { "eslint" }), "ESLint fix all", { buffer = event.buf })
      map("n", "<localleader>li", preferred_code_action("source.organizeImports", { "vtsls" }), "Organize imports", { buffer = event.buf })
      map("n", "<localleader>lm", preferred_code_action("source.addMissingImports.ts", { "vtsls" }), "Add missing imports", { buffer = event.buf })
      map("n", "<localleader>lu", preferred_code_action("source.removeUnused.ts", { "vtsls" }), "Remove unused code", { buffer = event.buf })
      map("n", "<localleader>lv", function()
        vim.lsp.buf.code_action({ apply = true })
      end, "Code actions", { buffer = event.buf })
      map("n", "<localleader>ls", function() show_frontend_clients(event.buf) end, "Show frontend clients", { buffer = event.buf })

      local ft = vim.bo[event.buf].filetype
      if ft == "vue" then
        map("n", "<localleader>vf", preferred_code_action("source.fixAll.eslint", { "eslint" }), "Vue fix all", { buffer = event.buf })
        map("n", "<localleader>vi", preferred_code_action("source.organizeImports", { "vtsls" }), "Vue organize imports", { buffer = event.buf })
        map("n", "<localleader>vm", preferred_code_action("source.addMissingImports.ts", { "vtsls" }), "Vue add imports", { buffer = event.buf })
        map("n", "<localleader>vu", preferred_code_action("source.removeUnused.ts", { "vtsls" }), "Vue remove unused", { buffer = event.buf })
        map("n", "<localleader>vs", function() show_frontend_clients(event.buf) end, "Vue clients", { buffer = event.buf })
        map("n", "<localleader>va", function()
          vim.lsp.buf.code_action({ apply = true })
        end, "Vue actions", { buffer = event.buf })
      else
        map("n", "<localleader>tf", preferred_code_action("source.fixAll.eslint", { "eslint" }), "TS/JS fix all", { buffer = event.buf })
        map("n", "<localleader>ti", preferred_code_action("source.organizeImports", { "vtsls" }), "TS/JS organize imports", { buffer = event.buf })
        map("n", "<localleader>tm", preferred_code_action("source.addMissingImports.ts", { "vtsls" }), "TS/JS add imports", { buffer = event.buf })
        map("n", "<localleader>tu", preferred_code_action("source.removeUnused.ts", { "vtsls" }), "TS/JS remove unused", { buffer = event.buf })
      end
    end,
  })

  vim.api.nvim_create_autocmd("BufWritePre", {
    group = vim.api.nvim_create_augroup("plugins_frontend_eslint_fix", { clear = true }),
    pattern = { "*.js", "*.jsx", "*.ts", "*.tsx", "*.vue", "*.css", "*.scss", "*.less", "*.html", "*.json", "*.jsonc", "*.yaml", "*.yml", "*.toml" },
    callback = function(event)
      local bufnr = event.buf
      local frontend = frontend_state.get()
      local save_actions = frontend.save_actions or {}

      if frontend.eslint_fix_on_save == false then return end

      if save_actions.eslint and has_client(bufnr, { "eslint" }) then
        if vim.fn.exists(":EslintFixAll") == 2 then
          pcall(vim.cmd, "silent EslintFixAll")
        else
          apply_sync_code_action(bufnr, "source.fixAll.eslint", { "eslint" })
        end
      end

      if save_actions.organize_imports and has_client(bufnr, { "vtsls" }) then
        apply_sync_code_action(bufnr, "source.organizeImports", { "vtsls" })
      end

      if save_actions.remove_unused and has_client(bufnr, { "vtsls" }) then
        apply_sync_code_action(bufnr, "source.removeUnused.ts", { "vtsls" })
      end

      if save_actions.add_missing_imports and has_client(bufnr, { "vtsls" }) then
        apply_sync_code_action(bufnr, "source.addMissingImports.ts", { "vtsls" })
      end
    end,
  })

  map("n", "<leader>ls", function()
    terminal.open_float("live-server", { title = " live-server " })
  end, "Live Server start")
end

return M
