local M = {}
local build = require "lang.cpp.build"
local cmake_root = build.cmake_root
local make_root = build.make_root
local preferred_make_target = build.preferred_make_target
local cpp_mode_label = build.mode_label

local function cpp_key_lines(bufnr)
  local file = vim.api.nvim_buf_get_name(bufnr)
  local lines = {
    "C/C++ keymaps",
    "",
    "Buffer: " .. (file ~= "" and file or "[No Name]"),
    "Mode:   " .. cpp_mode_label(bufnr),
    "",
    ",cb  Build current file / make / CMake target",
    ",cr  Run current file / make run / CMake target",
    ",cc  Clean current artifact / make clean / CMake target",
    ",ct  Run tests / make test / CTest",
    ",ch  Switch source/header",
    ",ca  Show Clangd AST",
    ",ci  Show Clangd symbol info",
    ",cT  Show Clangd type hierarchy",
    ",cm  Show Clangd memory usage",
  }

  local make_project_root = make_root(bufnr)
  if make_project_root then
    lines[#lines + 1] = ""
    lines[#lines + 1] = "Makefile mode"
    lines[#lines + 1] = ",cb  Uses default `make` target"
    lines[#lines + 1] = ",cr  "
      .. (preferred_make_target(make_project_root, { "run" }) and "Uses `make run`" or "Needs a `run:` target")
    lines[#lines + 1] = ",cc  "
      .. (
        preferred_make_target(make_project_root, { "clean", "distclean", "mrproper" }) and "Uses clean target"
        or "Needs a `clean:` target"
      )
    lines[#lines + 1] = ",ct  "
      .. (
        preferred_make_target(make_project_root, { "test", "check" }) and "Uses `make test/check`"
        or "Needs a `test:` or `check:` target"
      )
  end

  if cmake_root(bufnr) then
    lines[#lines + 1] = ""
    lines[#lines + 1] = "CMake file extras"
    lines[#lines + 1] = ",cg  Generate CMake project"
    lines[#lines + 1] = ",cs  Select build target"
    lines[#lines + 1] = ",cl  Select launch target"
    lines[#lines + 1] = ",cv  Select build type"
    lines[#lines + 1] = ",cp  Select configure preset"
  end

  return lines
end

function M.show_keys(bufnr)
  local lines = cpp_key_lines(bufnr or vim.api.nvim_get_current_buf())
  vim.notify(table.concat(lines, "\n"), vim.log.levels.INFO, { title = "CppKeys" })
end

return M
