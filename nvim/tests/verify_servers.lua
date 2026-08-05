local root = arg[0] and vim.fn.fnamemodify(arg[0], ":p:h:h") or nil
if root then vim.opt.rtp:prepend(root) end

local registry = require("lsp.servers").get()
local expected = {
  "bashls",
  "basedpyright",
  "cssls",
  "eslint",
  "emmet_language_server",
  "gopls",
  "jsonls",
  "lua_ls",
  "marksman",
  "ruff",
  "rust_analyzer",
  "taplo",
  "ty",
  "vtsls",
  "vue_ls",
  "yamlls",
  "zls",
}

local missing = {}
for _, name in ipairs(expected) do
  if not registry[name] then table.insert(missing, name) end
end

for _, name in ipairs(missing) do
  print("MISSING: " .. name)
end
for name in pairs(registry) do
  local found = false
  for _, expected_name in ipairs(expected) do
    if name == expected_name then
      found = true
      break
    end
  end
  if not found then print("EXTRA: " .. name) end
end

if #missing == 0 then
  print("OK all " .. #expected .. " servers present")
else
  vim.cmd "cquit"
end
