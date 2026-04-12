local M = {}

local util = require("core.util")

local function build_server_registry()
  local registry = require("lsp.servers").get()

  for _, entry in ipairs(util.load_table_modules("user.lsp")) do
    if entry.name and entry.config then registry[entry.name] = entry end
  end

  local user = require("user")
  for name, config in pairs(user.lsp.servers) do
    local current = registry[name]
    if current then
      current.config = vim.tbl_deep_extend("force", current.config, config)
      current.ensure_installed = current.ensure_installed or user.lsp.ensure_installed_lookup[name]
    else
      registry[name] = {
        name = name,
        config = config,
        ensure_installed = user.lsp.ensure_installed_lookup[name] or false,
      }
    end
  end

  return registry
end

function M.setup()
  if vim.env.NVIM_NO_PLUGINS == "1" then return end

  require("lang.python").setup()

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

  local map = require("core.keymaps").map
  map("n", "K", vim.lsp.buf.hover, "Hover documentation")
  map("n", "<leader>ca", vim.lsp.buf.code_action, "Code action")
  map("n", "<leader>cr", vim.lsp.buf.rename, "Rename symbol")
  map("n", "<leader>cl", vim.lsp.codelens.run, "Run codelens")
  map("n", "<leader>xd", vim.diagnostic.open_float, "Line diagnostics")
  map("n", "[d", vim.diagnostic.goto_prev, "Previous diagnostic")
  map("n", "]d", vim.diagnostic.goto_next, "Next diagnostic")
  map("n", "<leader>xq", vim.diagnostic.setqflist, "Diagnostics to quickfix")

  for _, lhs in ipairs({ "grr", "grn", "gra", "gri", "grt", "grx" }) do
    pcall(vim.keymap.del, "n", lhs)
    pcall(vim.keymap.del, "x", lhs)
  end
end

return M
