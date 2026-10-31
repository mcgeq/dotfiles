local M = {}

function M.setup()
  local map = require("core.keymaps").map
  local loader = require "pack.loader"
  local ready = false
  local roots = {}

  local function grug()
    if not loader.ensure "grug-far.nvim" then return end
    local ok, module = pcall(require, "grug-far")
    if not ok then return end
    if not ready then
      module.setup { engine = "ripgrep", enabledEngines = { "ripgrep" } }
      ready = true
    end
    return module
  end

  -- Paths uses backslash-escaped spaces, not shell quoting or glob syntax.
  local function path_input(path) return path:gsub(" ", "\\ ") end

  local function context()
    local root = roots[vim.api.nvim_get_current_buf()] or require("core.workspace").current_root(0)
    local id = vim.fn.sha256(root)
    return root, "project-replace-" .. id, vim.fn.stdpath "state" .. "/grug-far/" .. id
  end

  local function open(module, root, name, history, prefills)
    local instance = module.open {
      instanceName = name,
      history = { historyDir = history },
      visualSelectionUsage = "ignore",
      prefills = prefills or { paths = path_input(root) },
    }
    roots[instance:get_buf()] = root
    return instance
  end

  map("n", "<leader>sr", function()
    local module = grug()
    if not module then return end
    local root, name, history = context()
    if module.has_instance(name) then
      module.toggle_instance { instanceName = name }
    else
      open(module, root, name, history)
    end
  end, "Replace in files")

  map("n", "<leader>su", function()
    local module = grug()
    if not module then return end
    local root, name, history = context()
    if module.has_instance(name) then
      module.get_instance(name):open()
    else
      local entry = module.get_last_history_entry(history .. "/history")
      open(module, root, name, history, entry)
    end
  end, "Resume replace search")

  map("n", "<leader>sQ", function()
    local module = grug()
    if not module then return end
    local _, name = context()
    if module.has_instance(name) then module.get_instance(name):open_quickfix() end
  end, "Send replace results to quickfix")

  local function replace(visual, current_file)
    local module = grug()
    if not module then return end
    local root, name, history = context()
    local path = current_file and vim.api.nvim_buf_get_name(0) or root
    if path == "" or (current_file and vim.bo.buftype ~= "") then
      vim.notify("Open a named file before replacing in the current file.", vim.log.levels.WARN)
      return
    end
    -- Capture the source selection before opening/focusing the replace buffer.
    local search = visual and module.get_current_visual_selection() or vim.fn.expand "<cword>"
    local prefills = {
      search = search or "",
      paths = path_input(path),
      flags = "--fixed-strings" .. ((search or ""):find("\n", 1, true) and " --multiline" or ""),
    }
    if module.has_instance(name) then
      local instance = module.get_instance(name)
      instance:open()
      instance:update_input_values(prefills, true)
    else
      open(module, root, name, history, prefills)
    end
  end

  map("n", "<leader>sW", function() replace(false, false) end, "Replace current word")
  map("x", "<leader>sW", function() replace(true, false) end, "Replace selection")
  map("n", "<leader>sF", function() replace(false, true) end, "Replace in current file")
  map("x", "<leader>sF", function() replace(true, true) end, "Replace selection in current file")
end

return M
