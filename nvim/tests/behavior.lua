local source = debug.getinfo(1, "S").source:gsub("^@", "")
local root = vim.fn.fnamemodify(source, ":p:h:h")
vim.opt.rtp:prepend(root)

local failures, total = 0, 0
local restore = {}
local function stub(target, key, value)
  local original = target[key]
  restore[#restore + 1] = function() target[key] = original end
  target[key] = value
end

local function check(name, fn)
  total = total + 1
  local ok, err = xpcall(fn, debug.traceback)
  for i = #restore, 1, -1 do
    restore[i]()
  end
  restore = {}
  if ok then
    print("PASS " .. name)
  else
    failures = failures + 1
    io.stderr:write("FAIL " .. name .. "\n" .. err .. "\n")
  end
end

local function user(features)
  return {
    private = { features = features or {} },
    lang = {},
    pack = { disable = {}, add = {} },
    lsp = { ensure_installed = {}, auto_install = false },
  }
end

check("package-info maps multiple package buffers", function()
  stub(package.loaded, "plugins.web", nil)
  stub(package.loaded, "pack.loader", { ensure = function() return true end })
  local maps = 0
  stub(package.loaded, "core.keymaps", { map = function() maps = maps + 1 end })
  stub(package.loaded, "package-info", { setup = function() end, show = function() end })
  require("plugins.web").setup()
  local callback = vim.api.nvim_get_autocmds({ group = "plugins_package_info_keymaps" })[1].callback
  for _, path in ipairs { "/audit-a/package.json", "/audit-b/package.json" } do
    local buf = vim.api.nvim_create_buf(true, false)
    vim.api.nvim_buf_set_name(buf, path)
    callback { buf = buf }
    vim.api.nvim_buf_delete(buf, { force = true })
  end
  assert(maps == 14, "both buffers should receive their seven mappings")
end)

check("frontend custom save settings survive default mode selection", function()
  stub(package.loaded, "lang.frontend_state", nil)
  local config = user()
  config.lang.frontend = { eslint_fix_on_save = false, save_actions = { add_missing_imports = true } }
  stub(package.loaded, "user", config)
  local state = require("lang.frontend_state").get()
  assert(state.eslint_fix_on_save == false, "default mode must not overwrite user switches")
  assert(state.save_actions.add_missing_imports, "custom actions must remain enabled")
end)

check("format policy falls back when Biome is unavailable or filetype unsupported", function()
  stub(package.loaded, "user", user())
  stub(package.loaded, "lang.biome", {
    project_root = function() return "/biome-project" end,
    has_client = function() return false end,
    supports = function() return false end,
  })
  stub(vim.lsp, "get_clients", function() return {} end)
  local buf = vim.api.nvim_create_buf(false, true)
  vim.bo[buf].filetype = "python"
  assert(require("lang.formatting").provider(buf) == "conform")
  vim.bo[buf].filetype = "typescript"
  assert(require("lang.formatting").provider(buf) == "conform")
  vim.api.nvim_buf_delete(buf, { force = true })
end)

check("format-on-save master switch prevents frontend edits", function()
  stub(package.loaded, "user", user { format_on_save = false })
  stub(package.loaded, "lang.frontend_actions", { run_save_actions = function() error "unexpected edit" end })
  assert(require("lang.formatting").on_save(0) == false)
end)

check("ESLint fix switch does not disable TypeScript imports", function()
  stub(package.loaded, "lang.frontend_actions", nil)
  stub(package.loaded, "lang.biome", { project_root = function() end })
  local actions = require "lang.frontend_actions"
  stub(actions, "has_client", function(_, names) return names[1] == "vtsls" end)
  local requested
  stub(actions, "apply_sync_code_action", function(_, kind) requested = kind end)
  actions.run_save_actions(0, { eslint_fix_on_save = false, save_actions = { organize_imports = true } })
  assert(requested == "source.organizeImports", "imports are independent from ESLint fixes")
end)

check("manual formatting dispatches only to Biome in a Biome buffer", function()
  stub(package.loaded, "user", user())
  local count = 0
  stub(package.loaded, "lang.biome", {
    project_root = function() return "/biome-project" end,
    has_client = function() return true end,
    supports = function() return true end,
    format = function() count = count + 1 end,
  })
  local buf = vim.api.nvim_create_buf(false, true)
  vim.bo[buf].filetype = "typescript"
  require("lang.formatting").format(buf)
  assert(count == 1, "manual formatter must select Biome")
  vim.api.nvim_buf_delete(buf, { force = true })
end)

check("Python environment resolution leaves process environment unchanged", function()
  local python = require "lang.python"
  local fixture = vim.fn.tempname()
  vim.fn.mkdir(fixture .. "/.venv/Scripts", "p")
  vim.fn.mkdir(fixture .. "/.venv/bin", "p")
  vim.fn.writefile({}, fixture .. "/pyproject.toml")
  local executable = require("core.util").is_windows() and "/.venv/Scripts/python.exe" or "/.venv/bin/python"
  vim.fn.writefile({}, fixture .. executable)
  local before = { path = vim.env.PATH, venv = vim.env.VIRTUAL_ENV, provider = vim.g.python3_host_prog }
  local env = python.resolve(fixture .. "/example.py")
  assert(env and env.python == vim.fs.normalize(fixture .. executable), "project interpreter must be resolved")
  assert(vim.env.PATH == before.path and vim.env.VIRTUAL_ENV == before.venv, "process environment changed")
  assert(vim.g.python3_host_prog == before.provider, "provider changed")
  assert(python.resolve(vim.fn.tempname() .. "/outside.py") == nil, "unrelated projects must not inherit a venv")
end)

check("optional module execution errors are reported", function()
  local messages = {}
  stub(vim, "notify", function(msg) messages[#messages + 1] = msg end)
  stub(package.preload, "user.audit_broken", function() error "audit syntax failure" end)
  require("core.util").require_if_exists "user.audit_broken"
  assert(#messages == 1 and messages[1]:find("audit syntax failure", 1, true), "module errors must be visible")
end)

check("plugin disable and feature switches apply to loading", function()
  local config = user { noice = false }
  config.pack.disable = { "bufferline.nvim" }
  stub(package.loaded, "user", config)
  assert(require("pack.spec").enabled "noice.nvim" == false)
  assert(require("pack.spec").enabled "bufferline.nvim" == false)
  local calls = 0
  stub(vim.cmd, "packadd", function() calls = calls + 1 end)
  assert(require("pack.loader").ensure "noice.nvim" == false)
  assert(calls == 0, "disabled plugin must not be loaded from an existing installation")
end)

check("PackSync restores lockfile revisions offline", function()
  stub(package.loaded, "core.keymaps", { map = function() end })
  local options
  stub(vim.pack, "update", function(_, opts) options = opts end)
  require("pack.commands").setup()
  vim.cmd.PackSync()
  assert(options.target == "lockfile" and options.offline == true)
  options = nil
  vim.cmd.PackRestore()
  assert(options.target == "lockfile" and options.offline == true)
end)

check("C++ is supported by the shared runner", function()
  assert(require("lang.runner.ui").supports "cpp", "C++ runner is missing")
  assert(require("lang.runner.ui").supports "c", "C runner is missing")
end)

check("SQL completion loads Dadbod only when enabled", function()
  local config = user()
  stub(package.loaded, "user", config)
  local completion
  stub(package.loaded, "blink.cmp", { setup = function(opts) completion = opts end })
  stub(package.loaded, "pack.loader", { ensure_many = function() return true end })
  require("plugins.completion").setup()
  assert(completion.sources.providers and completion.sources.providers.dadbod, "Dadbod provider missing")
  assert(vim.list_contains(completion.sources.per_filetype.sql, "dadbod"), "SQL provider missing")
  config.private.features.database = false
  require("plugins.completion").setup()
  assert(not vim.list_contains(completion.sources.per_filetype.sql, "dadbod"), "Disabled database source loaded")
end)

check("snippet manifest references real files", function()
  local manifest = vim.json.decode(table.concat(vim.fn.readfile(root .. "/snippets/package.json"), "\n"))
  for _, snippet in ipairs(manifest.contributes.snippets) do
    assert(vim.fn.filereadable(root .. "/snippets/" .. snippet.path) == 1, "Missing snippet: " .. snippet.path)
  end
end)

check("Python tools are selected independently per project", function()
  local python = require "lang.python"
  stub(
    python,
    "resolve",
    function(path) return { bin_dir = path .. "/.venv/bin", python = path .. "/.venv/bin/python" } end
  )
  stub(vim.fn, "executable", function() return 1 end)
  local suffix = require("core.util").executable_suffix()
  assert(python.executable("ruff", "/project-a") == "/project-a/.venv/bin/ruff" .. suffix, "Project A tool lost")
  assert(python.executable("ruff", "/project-b") == "/project-b/.venv/bin/ruff" .. suffix, "Project B tool lost")
  local settings = {}
  local config = { root_dir = "/project-b", settings = settings }
  python.before_init({}, config)
  assert(
    config.settings == settings and settings.python.pythonPath:find("project-b", 1, true),
    "Client settings reference lost"
  )
end)

check("Mason integrations do not eagerly install LSP servers", function()
  local config = user()
  stub(package.loaded, "user", config)
  stub(package.loaded, "mason", { setup = function() end })
  local install_list
  stub(package.loaded, "mason-lspconfig", { setup = function(opts) install_list = opts.ensure_installed end })
  stub(package.loaded, "mason-tool-installer", { setup = function(opts) assert(opts.run_on_start == false) end })
  require("core.tools").setup()
  assert(install_list and #install_list == 0, "Duplicate LSP installation owner")
  local callbacks = 0
  stub(vim.api, "nvim_create_autocmd", function() callbacks = callbacks + 1 end)
  require("core.tools").setup_lsp {}
  assert(callbacks == 0, "auto_install=false must prevent installer registration")
end)

check("source actions resolve and execute on the responding client", function()
  local buf = vim.api.nvim_create_buf(false, true)
  local requests = {}
  local client = {
    name = "audit",
    offset_encoding = "utf-16",
    supports_method = function() return true end,
    request_sync = function(_, method)
      requests[#requests + 1] = method
      if method == "textDocument/codeAction" then return { result = { { title = "fix", data = {} } } } end
      if method == "codeAction/resolve" then return { result = { command = { command = "audit.fix" } } } end
      return { result = vim.NIL }
    end,
  }
  stub(vim.lsp, "get_clients", function() return { client } end)
  assert(require("lsp.actions").apply(buf, "source.fixAll", { "audit" }), "Source action was not applied")
  assert(
    table.concat(requests, ",") == "textDocument/codeAction,codeAction/resolve,workspace/executeCommand",
    "Wrong request order"
  )
  vim.api.nvim_buf_delete(buf, { force = true })
end)

check("disabled user plugins without explicit names skip setup", function()
  local config = user()
  config.pack.disable = { "audit.nvim" }
  stub(package.loaded, "user", config)
  stub(package.loaded, "core.tools", { setup = function() end })
  stub(require "pack.spec", "configurations", function() return {} end)
  local calls = 0
  local extensions = {
    { spec = "https://github.com/acme/audit.nvim", setup = function() calls = calls + 1 end },
    { spec = { src = "https://github.com/acme/audit.nvim" }, setup = function() calls = calls + 1 end },
  }
  stub(require "core.util", "load_table_modules", function() return extensions end)
  require("plugins").setup()
  assert(calls == 0, "Disabled extension setup still ran")
end)

check("Python checker selection is local to each project", function()
  local python = require "lang.python"
  stub(python, "resolve", function(path) return { bin_dir = path .. "/.venv/bin" } end)
  stub(vim.fn, "executable", function(path) return path:find("/has-ty/", 1, true) and 1 or 0 end)
  assert(python.checker "/has-ty" == "ty", "Project-local Ty was ignored")
  assert(python.checker "/without-ty" == "basedpyright", "Ty leaked across projects")
end)

check("repeating a CMake action after wiping its buffer reports an unavailable context", function()
  local buf = vim.api.nvim_create_buf(false, true)
  vim.api.nvim_buf_set_name(buf, "/audit-cmake/main.cpp")
  stub(vim.fs, "root", function() return "/audit-cmake" end)
  vim.api.nvim_create_user_command("CMakeBuild", function() error "stale CMake action executed" end, { force = true })
  local result = require("lang.cpp.build").command(buf, "build")
  vim.api.nvim_buf_delete(buf, { force = true })
  local notified = false
  stub(vim, "notify", function() notified = true end)
  require("lang.runner.exec").execute_result(result)
  assert(notified, "Missing context should be reported")
end)

check("unsupported C++ file tests do not fall through to Make build", function()
  local buf = vim.api.nvim_create_buf(false, true)
  stub(vim.fs, "root", function(_, markers) return markers[1] == "GNUmakefile" and "/audit-make" or nil end)
  local result, err = require("lang.cpp.build").command(buf, "test_file")
  assert(result == nil and err, "Unsupported test_file action must not build the project")
  vim.api.nvim_buf_delete(buf, { force = true })
end)

check("search plugin migration keeps inventory and feature ownership consistent", function()
  local names = {}
  for _, spec in ipairs(require("pack.spec").get_base_specs()) do
    names[spec.name] = spec
  end
  assert(names["grug-far.nvim"] and names["grug-far.nvim"].feature == "search_replace")
  assert(not names["nvim-spectre"] and not names["mason-nvim-dap.nvim"] and not names["headlines.nvim"])
  local configs = require("pack.spec").configurations()
  assert(
    vim.tbl_filter(function(config) return config.module == "search" and config.plugin == "grug-far.nvim" end, configs)[1]
  )
  stub(package.loaded, "user", user { search_replace = false })
  assert(require("pack.spec").enabled "grug-far.nvim" == false)
end)

check("debug setup loads adapters directly without Mason DAP bridge", function()
  stub(package.loaded, "plugins.debug", nil)
  local maps = {}
  stub(package.loaded, "core.keymaps", { map = function(_, lhs, callback) maps[lhs] = callback end })
  local loaded
  stub(package.loaded, "pack.loader", {
    ensure_many = function(names)
      loaded = names
      return true
    end,
  })
  stub(package.preload, "dap", function()
    return {
      adapters = {},
      configurations = {},
      continue = function() end,
      listeners = { after = { event_initialized = {} }, before = { event_terminated = {}, event_exited = {} } },
    }
  end)
  stub(package.preload, "dap.utils", function()
    return { pick_process = function() end }
  end)
  stub(package.preload, "nvim-dap-virtual-text", function()
    return { setup = function() end }
  end)
  stub(package.preload, "dapui", function()
    return { setup = function() end }
  end)
  stub(vim.fn, "executable", function() return 0 end)
  require("plugins.debug").setup()
  maps["<leader>Dc"]()
  assert(not vim.tbl_contains(loaded, "mason-nvim-dap.nvim"), "removed bridge was loaded")
  assert(vim.tbl_contains(loaded, "nvim-dap") and vim.tbl_contains(loaded, "nvim-dap-ui"), "DAP features were removed")
end)

check("editing keeps mini modules without overriding native commenting", function()
  stub(package.loaded, "plugins.editing", nil)
  local setups = {}
  local function module(name)
    return { setup = function() setups[name] = (setups[name] or 0) + 1 end }
  end
  stub(package.preload, "mini.pairs", function() return module "pairs" end)
  stub(package.preload, "mini.ai", function() return module "ai" end)
  stub(package.preload, "mini.surround", function() return module "surround" end)
  stub(package.preload, "mini.comment", function() return module "comment" end)
  stub(package.preload, "spider", function()
    return { setup = function() end, motion = function() end }
  end)
  stub(package.loaded, "core.keymaps", { map = function() end })
  require("plugins.editing").setup()
  assert(setups.pairs and setups.ai and setups.surround, "mini editing modules were removed")
  assert(not setups.comment, "mini.comment must not override native gc/gcc")
end)

check("legacy Spectre feature switch remains effective with explicit new switch taking priority", function()
  stub(package.loaded, "user", nil)
  stub(package.loaded, "user.private", { features = { spectre = false } })
  assert(require("user").private.features.search_replace == false)
  package.loaded.user = nil
  package.loaded["user.private"] = { features = { spectre = false, search_replace = true } }
  assert(require("user").private.features.search_replace == true)
end)

print(("%d/%d behavior checks passed"):format(total - failures, total))
if failures > 0 then os.exit(1) end
