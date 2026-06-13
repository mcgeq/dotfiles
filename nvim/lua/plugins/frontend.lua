local M = {}

function M.setup()
  local actions = require("lang.frontend_actions")
  local frontend_filetypes = require("lang.frontend_filetypes")
  local map = require("core.keymaps").map
  local loader = require("pack.loader")
  local terminal = require("core.terminal")
  local frontend_state = require("lang.frontend_state")
  local colorizer_ready = false
  local autotag_ready = false

  vim.api.nvim_create_autocmd("ColorScheme", {
    group = vim.api.nvim_create_augroup("plugins_frontend_vue_highlights", { clear = true }),
    callback = function()
      vim.api.nvim_set_hl(0, "@lsp.type.component", { link = "@type" })
    end,
  })
  vim.api.nvim_set_hl(0, "@lsp.type.component", { link = "@type" })

  local function ensure_colorizer()
    if colorizer_ready then return true end
    if not loader.ensure("nvim-colorizer.lua") then return false end
    local ok_colorizer, colorizer = pcall(require, "colorizer")
    if not ok_colorizer then return false end
    colorizer.setup({
      filetypes = frontend_filetypes.visual_filetypes,
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
    colorizer_ready = true
    return true
  end

  local function ensure_autotag()
    if autotag_ready then return true end
    if not loader.ensure("nvim-ts-autotag") then return false end
    local ok_autotag, autotag = pcall(require, "nvim-ts-autotag")
    if not ok_autotag then return false end
    autotag.setup({
      opts = {
        enable_close = true,
        enable_rename = true,
        enable_close_on_slash = false,
      },
    })
    autotag_ready = true
    return true
  end

  vim.api.nvim_create_autocmd("FileType", {
    group = vim.api.nvim_create_augroup("plugins_frontend_visuals", { clear = true }),
    pattern = frontend_filetypes.visual_filetypes,
    callback = function()
      ensure_colorizer()
      ensure_autotag()
    end,
  })

  vim.api.nvim_create_autocmd("FileType", {
    group = vim.api.nvim_create_augroup("plugins_frontend_lsp_keymaps", { clear = true }),
    pattern = frontend_filetypes.lsp_filetypes,
    callback = function(event)
      map("n", "<localleader>lf", actions.preferred_code_action("source.fixAll.eslint", { "eslint" }), "ESLint fix all", { buffer = event.buf })
      map("n", "<localleader>li", actions.preferred_code_action("source.organizeImports", { "vtsls" }), "Organize imports", { buffer = event.buf })
      map("n", "<localleader>lm", actions.preferred_code_action("source.addMissingImports.ts", { "vtsls" }), "Add missing imports", { buffer = event.buf })
      map("n", "<localleader>lu", actions.preferred_code_action("source.removeUnused.ts", { "vtsls" }), "Remove unused code", { buffer = event.buf })
      map("n", "<localleader>lv", function()
        vim.lsp.buf.code_action({ apply = true })
      end, "Code actions", { buffer = event.buf })
      map("n", "<localleader>ls", function() actions.show_frontend_clients(event.buf) end, "Show frontend clients", { buffer = event.buf })

      local ft = vim.bo[event.buf].filetype
      if ft == "vue" then
        map("n", "<localleader>vf", actions.preferred_code_action("source.fixAll.eslint", { "eslint" }), "Vue fix all", { buffer = event.buf })
        map("n", "<localleader>vi", actions.preferred_code_action("source.organizeImports", { "vtsls" }), "Vue organize imports", { buffer = event.buf })
        map("n", "<localleader>vm", actions.preferred_code_action("source.addMissingImports.ts", { "vtsls" }), "Vue add imports", { buffer = event.buf })
        map("n", "<localleader>vu", actions.preferred_code_action("source.removeUnused.ts", { "vtsls" }), "Vue remove unused", { buffer = event.buf })
        map("n", "<localleader>vs", function() actions.show_frontend_clients(event.buf) end, "Vue clients", { buffer = event.buf })
        map("n", "<localleader>va", function()
          vim.lsp.buf.code_action({ apply = true })
        end, "Vue actions", { buffer = event.buf })
      else
        map("n", "<localleader>tf", actions.preferred_code_action("source.fixAll.eslint", { "eslint" }), "TS/JS fix all", { buffer = event.buf })
        map("n", "<localleader>ti", actions.preferred_code_action("source.organizeImports", { "vtsls" }), "TS/JS organize imports", { buffer = event.buf })
        map("n", "<localleader>tm", actions.preferred_code_action("source.addMissingImports.ts", { "vtsls" }), "TS/JS add imports", { buffer = event.buf })
        map("n", "<localleader>tu", actions.preferred_code_action("source.removeUnused.ts", { "vtsls" }), "TS/JS remove unused", { buffer = event.buf })
      end
    end,
  })

  vim.api.nvim_create_autocmd("BufWritePre", {
    group = vim.api.nvim_create_augroup("plugins_frontend_eslint_fix", { clear = true }),
    pattern = { "*.js", "*.jsx", "*.ts", "*.tsx", "*.vue", "*.css", "*.scss", "*.less", "*.html", "*.json", "*.jsonc", "*.yaml", "*.yml", "*.toml" },
    callback = function(event)
      local bufnr = event.buf
      actions.run_save_actions(bufnr, frontend_state.get())
    end,
  })

  map("n", "<leader>ls", function()
    terminal.open_float("live-server", { title = " live-server " })
  end, "Live Server start")
end

return M
