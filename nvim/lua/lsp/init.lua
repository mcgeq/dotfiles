local M = {}

local util = require "core.util"

local function diagnostic_nav(count, severity)
  return function() vim.diagnostic.jump { count = count, severity = severity, float = true } end
end

local function diagnostics_to_quickfix() vim.diagnostic.setqflist { open = false } end

local function diagnostics_to_loclist() vim.diagnostic.setloclist { open = false } end

local function ensure_lookup(names) return util.to_lookup(names) end

local function build_server_registry()
  local registry = require("lsp.servers").get()
  local install_lookup = ensure_lookup(require("user").lsp.ensure_installed)

  for _, entry in ipairs(util.load_table_modules "user.lsp") do
    if entry.name and entry.config then
      if entry.ensure_installed == nil then entry.ensure_installed = install_lookup[entry.name] end
      registry[entry.name] = entry
    end
  end

  for name in pairs(install_lookup) do
    if registry[name] then registry[name].ensure_installed = true end
  end
  return registry
end

function M.setup()
  if vim.env.NVIM_NO_PLUGINS == "1" then return end

  local registry = build_server_registry()

  for name, entry in pairs(registry) do
    vim.lsp.config(name, entry.config)
    if entry.enable ~= false then vim.lsp.enable(name) end
  end

  require("core.tools").setup_lsp(registry)

  local map = require("core.keymaps").map
  map("n", "K", vim.lsp.buf.hover, "Hover documentation")
  map("n", "<leader>ca", vim.lsp.buf.code_action, "Code action")
  map("n", "<leader>cr", vim.lsp.buf.rename, "Rename symbol")
  map("n", "<leader>cl", vim.lsp.codelens.run, "Run codelens")
  map("n", "<leader>xd", vim.diagnostic.open_float, "Line diagnostics")
  map("n", "[d", diagnostic_nav(-1), "Previous diagnostic")
  map("n", "]d", diagnostic_nav(1), "Next diagnostic")
  map("n", "[e", diagnostic_nav(-1, vim.diagnostic.severity.ERROR), "Previous error")
  map("n", "]e", diagnostic_nav(1, vim.diagnostic.severity.ERROR), "Next error")
  map("n", "[w", diagnostic_nav(-1, vim.diagnostic.severity.WARN), "Previous warning")
  map("n", "]w", diagnostic_nav(1, vim.diagnostic.severity.WARN), "Next warning")
  map("n", "<leader>xq", diagnostics_to_quickfix, "Diagnostics to quickfix")
  map("n", "<leader>xl", diagnostics_to_loclist, "Diagnostics to location list")

  for _, lhs in ipairs { "grr", "grn", "gra", "gri", "grt", "grx" } do
    pcall(vim.keymap.del, "n", lhs)
    pcall(vim.keymap.del, "x", lhs)
  end
end

return M
