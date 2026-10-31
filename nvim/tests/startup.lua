-- Offline integration: use installed plugin code with disposable data/state.
local source = debug.getinfo(1, "S").source:gsub("^@", "")
local root = vim.fn.fnamemodify(source, ":p:h:h")
local plugin_root = assert(vim.env.NVIM_TEST_PACK_ROOT, "NVIM_TEST_PACK_ROOT must point to installed opt plugins")
vim.opt.rtp:prepend(root)
vim.opt.packpath:prepend(vim.fn.fnamemodify(plugin_root, ":h:h:h"))
vim.g.mapleader = " "
vim.g.maplocalleader = ","

local errors = {}
local original_notify = vim.notify
vim.notify = function(message, level, opts)
  if level == vim.log.levels.ERROR then errors[#errors + 1] = tostring(message) end
  original_notify(message, level, opts)
end

local user = require "user"
user.private.features.dashboard = false
user.private.features.noice = false
user.private.features.debug = false
user.lsp.auto_install = false
user.session.autowrite = false

-- Exercise pack's real load policy without modifying the installed repositories.
vim.pack.add = function(specs, opts)
  for _, spec in ipairs(specs) do
    local path = vim.fs.joinpath(plugin_root, spec.name)
    assert(vim.fn.isdirectory(path) == 1, "Plugin not installed: " .. spec.name)
    opts.load { spec = spec, path = path }
  end
end
-- Installation is deliberately outside an offline startup check.
vim.lsp.enable = function() end

require "core"
require("pack").setup()
local registry = require "mason-registry"
registry.refresh = function(callback)
  if callback then callback(true, {}) end
end
require("nvim-treesitter").install = function() end
require("plugins").setup()
require("lsp").setup()

for _, filetype in ipairs { "lua", "cpp", "cmake", "markdown", "typescript", "python", "sql" } do
  local buf = vim.api.nvim_create_buf(true, false)
  vim.api.nvim_set_current_buf(buf)
  vim.bo[buf].filetype = filetype
  if filetype == "cpp" then
    assert(vim.fn.exists ":CMakeBuild" == 2, "CMake must load even when debug is disabled")
    assert(vim.fn.maparg(",cb", "n") ~= "", "C++ compatibility mapping missing")
    assert(vim.fn.maparg(",rb", "n") ~= "", "C++ runner mapping missing")
  end
  if filetype == "sql" then assert(require("plugins.dadbod_source").new():enabled(), "SQL source unavailable") end
  vim.api.nvim_buf_delete(buf, { force = true })
end

do
  local fixture = vim.fn.tempname()
  vim.fn.mkdir(fixture, "p")
  local file = fixture .. "/search fixture.txt"
  vim.fn.writefile({ "before needle after", "needle twice needle" }, file)
  local sourceBuf = vim.api.nvim_create_buf(true, false)
  vim.api.nvim_buf_set_name(sourceBuf, file)
  vim.api.nvim_set_current_buf(sourceBuf)
  vim.bo[sourceBuf].filetype = "text"
  vim.api.nvim_buf_set_lines(sourceBuf, 0, -1, false, { "before needle after", "needle twice needle" })
  vim.api.nvim_win_set_cursor(0, { 1, 8 })
  assert(require("pack.loader").ensure "grug-far.nvim", "grug-far did not lazy-load")
  require("plugins.search").setup()
  local callback = vim.fn.maparg("<leader>sF", "n", false, true).callback
  assert(type(callback) == "function", "current-file search mapping missing")
  callback()
  vim.wait(100)
  local instances = require "grug-far.instances"
  local instance = instances.get_instance(0)
  assert(instance, "grug-far current-file search did not open")
  local values = require("grug-far.inputs").getValues(instance._context, instance:get_buf())
  assert(values.search == "needle", "normal-mode word was not captured")
  assert(values.paths:find "search\\ fixture%.txt", "space-containing file path was not escaped")
  instance:close()
  vim.wait(100)
  vim.fn.delete(fixture, "rf")
end

assert(vim.fn.exists ":PackRestore" == 2, "PackRestore command missing")
assert(vim.fn.exists ":RunnerBuild" == 2, "Runner command missing")
assert(#vim.api.nvim_get_autocmds { group = "lang_formatting", event = "BufWritePre" } == 1, "Save entry is duplicated")
assert(package.loaded.noice == nil, "Disabled Noice was loaded")
vim.wait(100)
assert(#errors == 0, table.concat(errors, "\n"))
assert(vim.v.errmsg == "", vim.v.errmsg)
print "OK offline plugin setup and seven filetype integrations"
vim.cmd "qa!"
