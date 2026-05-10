local M = {
  spec = {
    { src = "https://github.com/p00f/clangd_extensions.nvim", name = "clangd_extensions.nvim" },
    { src = "https://github.com/Civitasv/cmake-tools.nvim", name = "cmake-tools.nvim" },
  },
}

local function setup_clangd_extensions()
  local ok, clangd_extensions = pcall(require, "clangd_extensions")
  if not ok then return end

  clangd_extensions.setup({
    memory_usage = {
      border = "rounded",
    },
    symbol_info = {
      border = "rounded",
    },
  })
end

local function setup_cmake_tools()
  local ok, cmake_tools = pcall(require, "cmake-tools")
  if not ok then return end

  cmake_tools.setup({
    cmake_use_preset = true,
    cmake_regenerate_on_save = true,
    cmake_generate_options = { "-DCMAKE_EXPORT_COMPILE_COMMANDS=1" },
    cmake_compile_commands_options = {
      action = "copy",
      target = vim.loop.cwd,
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
  })
end

function M.setup()
  local map = require("core.keymaps").map

  setup_clangd_extensions()
  setup_cmake_tools()

  vim.api.nvim_create_autocmd("FileType", {
    group = vim.api.nvim_create_augroup("user_cpp_keymaps", { clear = true }),
    pattern = { "c", "cpp", "objc", "objcpp", "cuda" },
    callback = function(event)
      if vim.fn.exists(":ClangdSwitchSourceHeader") == 2 then
        map("n", "<localleader>ch", "<cmd>ClangdSwitchSourceHeader<cr>", "Switch source/header", { buffer = event.buf })
      end
      if vim.fn.exists(":ClangdAST") == 2 then
        map("n", "<localleader>ca", "<cmd>ClangdAST<cr>", "Show AST", { buffer = event.buf })
      end
      if vim.fn.exists(":ClangdSymbolInfo") == 2 then
        map("n", "<localleader>ci", "<cmd>ClangdSymbolInfo<cr>", "Show symbol info", { buffer = event.buf })
      end
      if vim.fn.exists(":ClangdTypeHierarchy") == 2 then
        map("n", "<localleader>ct", "<cmd>ClangdTypeHierarchy<cr>", "Show type hierarchy", { buffer = event.buf })
      end
      if vim.fn.exists(":ClangdMemoryUsage") == 2 then
        map("n", "<localleader>cm", "<cmd>ClangdMemoryUsage<cr>", "Show memory usage", { buffer = event.buf })
      end
    end,
  })

  vim.api.nvim_create_autocmd("FileType", {
    group = vim.api.nvim_create_augroup("user_cmake_keymaps", { clear = true }),
    pattern = { "cmake" },
    callback = function(event)
      if vim.fn.exists(":CMakeGenerate") == 2 then
        map("n", "<localleader>cg", "<cmd>CMakeGenerate<cr>", "Generate CMake project", { buffer = event.buf })
      end
      if vim.fn.exists(":CMakeBuild") == 2 then
        map("n", "<localleader>cb", "<cmd>CMakeBuild<cr>", "Build current target", { buffer = event.buf })
      end
      if vim.fn.exists(":CMakeRun") == 2 then
        map("n", "<localleader>cr", "<cmd>CMakeRun<cr>", "Run current target", { buffer = event.buf })
      end
      if vim.fn.exists(":CMakeRunTest") == 2 then
        map("n", "<localleader>ct", "<cmd>CMakeRunTest<cr>", "Run tests", { buffer = event.buf })
      end
      if vim.fn.exists(":CMakeSelectBuildTarget") == 2 then
        map("n", "<localleader>cs", "<cmd>CMakeSelectBuildTarget<cr>", "Select build target", { buffer = event.buf })
      end
      if vim.fn.exists(":CMakeSelectLaunchTarget") == 2 then
        map("n", "<localleader>cl", "<cmd>CMakeSelectLaunchTarget<cr>", "Select launch target", { buffer = event.buf })
      end
      if vim.fn.exists(":CMakeSelectBuildType") == 2 then
        map("n", "<localleader>cv", "<cmd>CMakeSelectBuildType<cr>", "Select build type", { buffer = event.buf })
      end
      if vim.fn.exists(":CMakeSelectConfigurePreset") == 2 then
        map("n", "<localleader>cp", "<cmd>CMakeSelectConfigurePreset<cr>", "Select configure preset", { buffer = event.buf })
      end
    end,
  })
end

return M
