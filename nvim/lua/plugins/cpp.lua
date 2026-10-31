local M = {}
local function show_cpp_keys(bufnr) require("lang.cpp.ui").show_keys(bufnr) end
local function build_or_run_file(bufnr, run) require("lang.runner").run(run and "run" or "build", bufnr) end
local function clean_file_or_target(bufnr) require("lang.runner").run("clean", bufnr) end
local function run_tests(bufnr) require("lang.runner").run("test", bufnr) end

local function register_cpp_which_key(bufnr)
  local ok, which_key = pcall(require, "which-key")
  if not ok then return end

  which_key.add({
    { "<localleader>c", group = "C/C++" },
    { "<localleader>ca", desc = "Clangd AST" },
    { "<localleader>cb", desc = "Build" },
    { "<localleader>cc", desc = "Clean" },
    { "<localleader>ch", desc = "Switch Header/Source" },
    { "<localleader>ci", desc = "Clangd Symbol Info" },
    { "<localleader>ck", desc = "Show C/C++ Keymaps" },
    { "<localleader>cm", desc = "Clangd Memory Usage" },
    { "<localleader>cr", desc = "Run" },
    { "<localleader>ct", desc = "Run Tests" },
    { "<localleader>cT", desc = "Clangd Type Hierarchy" },
  }, { buffer = bufnr })
end

local function register_cmake_which_key(bufnr)
  local ok, which_key = pcall(require, "which-key")
  if not ok then return end

  which_key.add({
    { "<localleader>c", group = "CMake" },
    { "<localleader>cb", desc = "Build" },
    { "<localleader>cg", desc = "Generate" },
    { "<localleader>cl", desc = "Select Launch Target" },
    { "<localleader>cp", desc = "Select Configure Preset" },
    { "<localleader>cr", desc = "Run" },
    { "<localleader>cs", desc = "Select Build Target" },
    { "<localleader>ct", desc = "Run Tests" },
    { "<localleader>cv", desc = "Select Build Type" },
  }, { buffer = bufnr })
end

local function setup_clangd_extensions()
  local ok, clangd_extensions = pcall(require, "clangd_extensions")
  if not ok then return end

  clangd_extensions.setup {
    memory_usage = {
      border = "rounded",
    },
    symbol_info = {
      border = "rounded",
    },
  }
end

local function setup_cmake_tools()
  local ok, cmake_tools = pcall(require, "cmake-tools")
  if not ok then return end

  cmake_tools.setup {
    cmake_use_preset = true,
    cmake_regenerate_on_save = true,
    cmake_generate_options = { "-DCMAKE_EXPORT_COMPILE_COMMANDS=1" },
    cmake_compile_commands_options = {
      action = "copy",
      target = vim.uv.cwd,
    },
    cmake_executor = {
      name = "quickfix",
      default_opts = {
        quickfix = {
          show = "only_on_error",
          position = "belowright",
          size = 10,
          encoding = "utf-8",
          auto_close_when_success = true,
        },
      },
    },
    cmake_runner = {
      name = "terminal",
      default_opts = {
        terminal = {
          name = "Main Terminal",
          prefix_name = "[CMakeTools]: ",
          split_direction = "horizontal",
          split_size = 11,
          single_terminal_per_instance = true,
          single_terminal_per_tab = true,
          keep_terminal_static_location = true,
          auto_resize = true,
          start_insert = false,
          focus = false,
          do_not_add_newline = false,
        },
      },
    },
    cmake_virtual_text_support = true,
    cmake_use_scratch_buffer = false,
  }
end

function M.setup()
  local map = require("core.keymaps").map

  local ready = false
  local function ensure_plugins()
    if ready then return end
    local loader = require "pack.loader"
    if loader.ensure "clangd_extensions.nvim" then setup_clangd_extensions() end
    if loader.ensure_many { "plenary.nvim", "cmake-tools.nvim" } then setup_cmake_tools() end
    ready = true
  end

  vim.api.nvim_create_user_command(
    "CppKeys",
    function() show_cpp_keys(vim.api.nvim_get_current_buf()) end,
    { desc = "Show C/C++ keymaps" }
  )

  vim.api.nvim_create_autocmd("FileType", {
    group = vim.api.nvim_create_augroup("user_cpp_keymaps", { clear = true }),
    pattern = { "c", "cpp", "objc", "objcpp", "cuda" },
    callback = function(event)
      ensure_plugins()
      register_cpp_which_key(event.buf)
      map("n", "<localleader>ck", function() show_cpp_keys(event.buf) end, "Show C/C++ keymaps", { buffer = event.buf })
      map(
        "n",
        "<localleader>cb",
        function() build_or_run_file(event.buf, false) end,
        "Build file or CMake target",
        { buffer = event.buf }
      )
      map(
        "n",
        "<localleader>cr",
        function() build_or_run_file(event.buf, true) end,
        "Run file or CMake target",
        { buffer = event.buf }
      )
      map(
        "n",
        "<localleader>cc",
        function() clean_file_or_target(event.buf) end,
        "Clean file or CMake target",
        { buffer = event.buf }
      )
      map("n", "<localleader>ct", function() run_tests(event.buf) end, "Run tests", { buffer = event.buf })

      if vim.fn.exists ":ClangdSwitchSourceHeader" == 2 then
        map("n", "<localleader>ch", "<cmd>ClangdSwitchSourceHeader<cr>", "Switch source/header", { buffer = event.buf })
      end
      if vim.fn.exists ":ClangdAST" == 2 then
        map("n", "<localleader>ca", "<cmd>ClangdAST<cr>", "Show AST", { buffer = event.buf })
      end
      if vim.fn.exists ":ClangdSymbolInfo" == 2 then
        map("n", "<localleader>ci", "<cmd>ClangdSymbolInfo<cr>", "Show symbol info", { buffer = event.buf })
      end
      if vim.fn.exists ":ClangdTypeHierarchy" == 2 then
        map("n", "<localleader>cT", "<cmd>ClangdTypeHierarchy<cr>", "Show type hierarchy", { buffer = event.buf })
      end
      if vim.fn.exists ":ClangdMemoryUsage" == 2 then
        map("n", "<localleader>cm", "<cmd>ClangdMemoryUsage<cr>", "Show memory usage", { buffer = event.buf })
      end
    end,
  })

  vim.api.nvim_create_autocmd("FileType", {
    group = vim.api.nvim_create_augroup("user_cmake_keymaps", { clear = true }),
    pattern = { "cmake" },
    callback = function(event)
      ensure_plugins()
      register_cmake_which_key(event.buf)
      if vim.fn.exists ":CMakeGenerate" == 2 then
        map("n", "<localleader>cg", "<cmd>CMakeGenerate<cr>", "Generate CMake project", { buffer = event.buf })
      end
      if vim.fn.exists ":CMakeBuild" == 2 then
        map(
          "n",
          "<localleader>cb",
          function() build_or_run_file(event.buf, false) end,
          "Build current target",
          { buffer = event.buf }
        )
      end
      if vim.fn.exists ":CMakeRun" == 2 then
        map(
          "n",
          "<localleader>cr",
          function() build_or_run_file(event.buf, true) end,
          "Run current target",
          { buffer = event.buf }
        )
      end
      if vim.fn.exists ":CMakeRunTest" == 2 then
        map("n", "<localleader>ct", function() run_tests(event.buf) end, "Run tests", { buffer = event.buf })
      end
      if vim.fn.exists ":CMakeSelectBuildTarget" == 2 then
        map("n", "<localleader>cs", "<cmd>CMakeSelectBuildTarget<cr>", "Select build target", { buffer = event.buf })
      end
      if vim.fn.exists ":CMakeSelectLaunchTarget" == 2 then
        map("n", "<localleader>cl", "<cmd>CMakeSelectLaunchTarget<cr>", "Select launch target", { buffer = event.buf })
      end
      if vim.fn.exists ":CMakeSelectBuildType" == 2 then
        map("n", "<localleader>cv", "<cmd>CMakeSelectBuildType<cr>", "Select build type", { buffer = event.buf })
      end
      if vim.fn.exists ":CMakeSelectConfigurePreset" == 2 then
        map(
          "n",
          "<localleader>cp",
          "<cmd>CMakeSelectConfigurePreset<cr>",
          "Select configure preset",
          { buffer = event.buf }
        )
      end
    end,
  })
end

return M
