local M = {}

local util = require("core.util")

local MASON_PACKAGE_NAMES = {
  bashls = "bash-language-server",
  cssls = "css-lsp",
  emmet_language_server = "emmet-language-server",
  eslint = "eslint-lsp",
  jsonls = "json-lsp",
  lua_ls = "lua-language-server",
  rust_analyzer = "rust-analyzer",
  vue_ls = "vue-language-server",
  yamlls = "yaml-language-server",
}

local DEFAULT_FILETYPES = {
  basedpyright = { "python" },
  bashls = { "sh", "bash", "zsh" },
  clangd = { "c", "cpp", "objc", "objcpp", "cuda" },
  cssls = { "css", "scss", "less" },
  gopls = { "go", "gomod", "gowork", "gotmpl" },
  jsonls = { "json", "jsonc" },
  lua_ls = { "lua" },
  marksman = { "markdown", "markdown.mdx" },
  ruff = { "python" },
  rust_analyzer = { "rust" },
  taplo = { "toml" },
  yamlls = { "yaml" },
  zls = { "zig", "zir" },
}

local function diagnostic_nav(next_fn, severity)
  return function()
    next_fn({
      severity = severity,
    })
  end
end

local function diagnostics_to_quickfix()
  vim.diagnostic.setqflist({ open = false })
end

local function diagnostics_to_loclist()
  vim.diagnostic.setloclist({ open = false })
end

local function ensure_lookup(names)
  return util.to_lookup(names)
end

local function build_server_registry()
  local registry = require("lsp.servers").get()
  local install_lookup = ensure_lookup(require("user").lsp.ensure_installed)

  for _, entry in ipairs(util.load_table_modules("user.lsp")) do
    if entry.name and entry.config then
      if entry.ensure_installed == nil then
        entry.ensure_installed = install_lookup[entry.name]
      end
      registry[entry.name] = entry
    end
  end

  return registry
end

local function server_filetypes(name, entry)
  local config = entry.config or {}
  return config.filetypes or DEFAULT_FILETYPES[name] or {}
end

local function build_filetype_server_index(registry)
  local index = {}

  for name, entry in pairs(registry) do
    if entry.ensure_installed and entry.enable ~= false then
      for _, filetype in ipairs(server_filetypes(name, entry)) do
        index[filetype] = index[filetype] or {}
        table.insert(index[filetype], name)
      end
    end
  end

  for _, names in pairs(index) do
    table.sort(names)
  end

  return index
end

local function mason_package_name(name)
  return MASON_PACKAGE_NAMES[name] or name
end

local function retry_lsp_attach(name, bufnr)
  if not vim.api.nvim_buf_is_valid(bufnr) then return end
  vim.lsp.enable(name)
  pcall(vim.api.nvim_exec_autocmds, "FileType", {
    group = "nvim.lsp.enable",
    buffer = bufnr,
    modeline = false,
  })
end

local function setup_mason_auto_install(registry)
  if #vim.api.nvim_list_uis() == 0 then return end

  local filetype_servers = build_filetype_server_index(registry)
  local installing = {}
  local registry_ready = false
  local registry_refreshing = false
  local registry_callbacks = {}

  local function with_mason_registry(callback)
    local ok_mason, mason = pcall(require, "mason")
    if ok_mason and not mason.has_setup then
      mason.setup({
        ui = {
          border = "rounded",
        },
      })
    end

    local ok_registry, mason_registry = pcall(require, "mason-registry")
    if not ok_registry then return end

    if registry_ready then
      callback(mason_registry)
      return
    end

    table.insert(registry_callbacks, callback)
    if registry_refreshing then return end

    registry_refreshing = true
    mason_registry.refresh(function(success)
      registry_ready = success
      registry_refreshing = false

      local callbacks = registry_callbacks
      registry_callbacks = {}

      if not success then
        vim.schedule(function()
          vim.notify("Mason registry refresh failed; LSP auto-install skipped.", vim.log.levels.WARN, {
            title = "Mason",
          })
        end)
        return
      end

      for _, cb in ipairs(callbacks) do
        cb(mason_registry)
      end
    end)
  end

  local function ensure_server_installed(name, bufnr)
    local package_name = mason_package_name(name)
    if installing[package_name] then return end

    with_mason_registry(function(mason_registry)
      local ok_package, package = pcall(mason_registry.get_package, package_name)
      if not ok_package then
        vim.schedule(function()
          vim.notify(("Mason package not found for `%s`: %s"):format(name, package_name), vim.log.levels.WARN, {
            title = "Mason",
          })
        end)
        return
      end

      if package:is_installed() then
        retry_lsp_attach(name, bufnr)
        return
      end
      if package:is_installing() then return end

      installing[package_name] = true
      vim.schedule(function()
        vim.notify(("Installing `%s` for `%s`..."):format(package_name, name), vim.log.levels.INFO, {
          title = "Mason",
        })
      end)

      local ok_install, err = pcall(function()
        package:install({}, function(success, result)
          installing[package_name] = nil

          vim.schedule(function()
            if success then
              vim.notify(("Installed `%s`."):format(package_name), vim.log.levels.INFO, {
                title = "Mason",
              })
              retry_lsp_attach(name, bufnr)
            else
              vim.notify(("Failed to install `%s`: %s"):format(package_name, result), vim.log.levels.ERROR, {
                title = "Mason",
              })
            end
          end)
        end)
      end)

      if not ok_install then
        installing[package_name] = nil
        vim.schedule(function()
          vim.notify(("Failed to start Mason install for `%s`: %s"):format(package_name, err), vim.log.levels.ERROR, {
            title = "Mason",
          })
        end)
      end
    end)
  end

  local function ensure_buffer_servers(bufnr)
    if not vim.api.nvim_buf_is_valid(bufnr) then return end

    local names = filetype_servers[vim.bo[bufnr].filetype]
    if not names then return end

    for _, name in ipairs(names) do
      ensure_server_installed(name, bufnr)
    end
  end

  vim.api.nvim_create_autocmd("FileType", {
    group = vim.api.nvim_create_augroup("lsp_mason_auto_install", { clear = true }),
    callback = function(event) ensure_buffer_servers(event.buf) end,
  })

  vim.schedule(function()
    for _, bufnr in ipairs(vim.api.nvim_list_bufs()) do
      ensure_buffer_servers(bufnr)
    end
  end)
end

function M.setup()
  if vim.env.NVIM_NO_PLUGINS == "1" then return end

  local python = require("lang.python")
  python.setup()
  python.prime()

  local registry = build_server_registry()

  local ensure_installed = {}
  for name, entry in pairs(registry) do
    if entry.ensure_installed then table.insert(ensure_installed, name) end
  end
  table.sort(ensure_installed)

  local ok_mason_lsp, mason_lsp = pcall(require, "mason-lspconfig")
  if ok_mason_lsp then
    mason_lsp.setup({
      ensure_installed = ensure_installed,
      automatic_enable = false,
    })
  end

  for name, entry in pairs(registry) do
    vim.lsp.config(name, entry.config)
    if entry.enable ~= false then vim.lsp.enable(name) end
  end

  local user = require("user")
  if user.lsp.auto_install ~= false then
    setup_mason_auto_install(registry)
  end

  local map = require("core.keymaps").map
  map("n", "K", vim.lsp.buf.hover, "Hover documentation")
  map("n", "<leader>ca", vim.lsp.buf.code_action, "Code action")
  map("n", "<leader>cr", vim.lsp.buf.rename, "Rename symbol")
  map("n", "<leader>cl", vim.lsp.codelens.run, "Run codelens")
  map("n", "<leader>xd", vim.diagnostic.open_float, "Line diagnostics")
  map("n", "[d", vim.diagnostic.goto_prev, "Previous diagnostic")
  map("n", "]d", vim.diagnostic.goto_next, "Next diagnostic")
  map("n", "[e", diagnostic_nav(vim.diagnostic.goto_prev, vim.diagnostic.severity.ERROR), "Previous error")
  map("n", "]e", diagnostic_nav(vim.diagnostic.goto_next, vim.diagnostic.severity.ERROR), "Next error")
  map("n", "[w", diagnostic_nav(vim.diagnostic.goto_prev, vim.diagnostic.severity.WARN), "Previous warning")
  map("n", "]w", diagnostic_nav(vim.diagnostic.goto_next, vim.diagnostic.severity.WARN), "Next warning")
  map("n", "<leader>xq", diagnostics_to_quickfix, "Diagnostics to quickfix")
  map("n", "<leader>xl", diagnostics_to_loclist, "Diagnostics to location list")

  for _, lhs in ipairs({ "grr", "grn", "gra", "gri", "grt", "grx" }) do
    pcall(vim.keymap.del, "n", lhs)
    pcall(vim.keymap.del, "x", lhs)
  end
end

return M
