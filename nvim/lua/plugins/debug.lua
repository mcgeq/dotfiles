local M = {}

local util = require("core.util")
local first_executable = util.first_executable

local function mason_package_path(name)
  return vim.fs.joinpath(vim.fn.stdpath("data"), "mason", "packages", name)
end

local function codelldb_adapter()
  local package = mason_package_path("codelldb")
  local adapter = util.is_windows() and "extension\\adapter\\codelldb.exe" or "extension/adapter/codelldb"
  local candidate = vim.fs.joinpath(package, adapter)
  if vim.fn.executable(candidate) == 1 then return candidate end
  return first_executable({ "codelldb" })
end

local function python_adapter()
  local mason_python = util.is_windows() and "venv\\Scripts\\python.exe" or "venv/bin/python"
  local candidate = vim.fs.joinpath(mason_package_path("debugpy"), mason_python)
  if vim.fn.executable(candidate) == 1 then return { candidate, "-m", "debugpy.adapter" } end
  if vim.fn.executable("uv") == 1 then return { "uv", "run", "python", "-m", "debugpy.adapter" } end
  local python = first_executable({ "python", "python3" }) or "python"
  return { python, "-m", "debugpy.adapter" }
end

local function setup_dap()
  local ok_dap, dap = pcall(require, "dap")
  if not ok_dap then return end

  local codelldb = codelldb_adapter()
  if codelldb then
    dap.adapters.codelldb = {
      type = "server",
      port = "${port}",
      executable = {
        command = codelldb,
        args = { "--port", "${port}" },
      },
    }

    local c_family = {
      {
        name = "Launch executable",
        type = "codelldb",
        request = "launch",
        program = function()
          return vim.fn.input("Executable: ", vim.fn.getcwd() .. "/", "file")
        end,
        cwd = "${workspaceFolder}",
        stopOnEntry = false,
      },
      {
        name = "Attach to process",
        type = "codelldb",
        request = "attach",
        pid = require("dap.utils").pick_process,
        cwd = "${workspaceFolder}",
      },
    }
    dap.configurations.c = c_family
    dap.configurations.cpp = c_family
    dap.configurations.rust = c_family
    dap.configurations.zig = c_family
  end

  local debugpy = python_adapter()
  dap.adapters.python = {
    type = "executable",
    command = debugpy[1],
    args = vim.list_slice(debugpy, 2),
  }
  dap.configurations.python = {
    {
      type = "python",
      request = "launch",
      name = "Launch current file",
      program = "${file}",
      console = "integratedTerminal",
      justMyCode = false,
    },
  }
end

function M.setup()
  local map = require("core.keymaps").map
  local loader = require("pack.loader")
  local ready = false

  local function ensure_debug()
    if ready then return true end
    if not loader.ensure_many({
      "mason-nvim-dap.nvim",
      "nvim-dap",
      "nvim-dap-ui",
      "nvim-dap-virtual-text",
      "nvim-nio",
    }) then
      return false
    end

    local ok_mason_dap, mason_dap = pcall(require, "mason-nvim-dap")
    if ok_mason_dap then
      mason_dap.setup({
        ensure_installed = { "codelldb", "debugpy" },
        automatic_installation = false,
        handlers = {},
      })
    end

    setup_dap()

    local ok_virtual_text, virtual_text = pcall(require, "nvim-dap-virtual-text")
    if ok_virtual_text then virtual_text.setup({ commented = true }) end

    local ok_dapui, dapui = pcall(require, "dapui")
    if ok_dapui then
      dapui.setup()
      local ok_dap, dap = pcall(require, "dap")
      if ok_dap then
        dap.listeners.after.event_initialized["dapui"] = function() dapui.open() end
        dap.listeners.before.event_terminated["dapui"] = function() dapui.close() end
        dap.listeners.before.event_exited["dapui"] = function() dapui.close() end
      end
    end

    ready = true
    return true
  end

  local function with_dap(fn)
    return function()
      if not ensure_debug() then return end
      local ok_dap, dap = pcall(require, "dap")
      if not ok_dap then return end
      fn(dap)
    end
  end

  local function with_dapui(fn)
    return function()
      if not ensure_debug() then return end
      local ok_dapui, dapui = pcall(require, "dapui")
      if not ok_dapui then return end
      fn(dapui)
    end
  end

  map("n", "<leader>Dc", with_dap(function(dap) dap.continue() end), "Debug continue")
  map("n", "<leader>DC", with_dap(function(dap) dap.run_to_cursor() end), "Debug to cursor")
  map("n", "<leader>Db", with_dap(function(dap) dap.toggle_breakpoint() end), "Debug breakpoint")
  map("n", "<leader>DB", with_dap(function(dap)
    dap.set_breakpoint(vim.fn.input("Breakpoint condition: "))
  end), "Debug conditional breakpoint")
  map("n", "<leader>Do", with_dap(function(dap) dap.step_over() end), "Debug step over")
  map("n", "<leader>Di", with_dap(function(dap) dap.step_into() end), "Debug step into")
  map("n", "<leader>DO", with_dap(function(dap) dap.step_out() end), "Debug step out")
  map("n", "<leader>Dr", with_dap(function(dap) dap.repl.open() end), "Debug REPL")
  map("n", "<leader>Dl", with_dap(function(dap) dap.run_last() end), "Debug run last")
  map("n", "<leader>Dt", with_dap(function(dap) dap.terminate() end), "Debug terminate")
  map("n", "<leader>Du", with_dapui(function(dapui) dapui.toggle() end), "Debug UI")
end

return M
