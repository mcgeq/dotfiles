local M = {}

local util = require "core.util"
local is_windows = util.is_windows
local executable_suffix = util.executable_suffix
local shell_quote = util.shell_quote
local powershell_quote = util.powershell_quote

local function is_usable_executable_path(path)
  if type(path) ~= "string" or path == "" then return false end
  if not is_windows() then return true end

  local lower = path:lower()
  return path:match "^[A-Za-z]:[\\/]"
    or path:match "^\\\\"
    or lower:match "%.exe$"
    or lower:match "%.cmd$"
    or lower:match "%.bat$"
end

local function first_executable(names)
  for _, name in ipairs(names) do
    local path = vim.fn.exepath(name)
    if is_usable_executable_path(path) then return path end
    if vim.fn.executable(name) == 1 then return name end
  end
end

local function shell_executable(value)
  if is_windows() then return "& " .. util.powershell_quote(value) end
  return shell_quote(value)
end

local exec = require "lang.runner.exec"
local function shell_command(command, cwd, title) return exec.shell_command(command, cwd, title) end

local function cmake_root(bufnr)
  return vim.fs.root(bufnr, {
    "CMakeLists.txt",
    "CMakePresets.json",
    "CMakeUserPresets.json",
  })
end

local function expand_cmake_path(value, root)
  if type(value) ~= "string" or value == "" then return nil end

  return value
    :gsub("%${sourceDir}", root)
    :gsub("%${sourceParentDir}", vim.fn.fnamemodify(root, ":h"))
    :gsub("%${sourceDirName}", vim.fn.fnamemodify(root, ":t"))
end

local function cmake_preset_binary_dir(root, preset)
  local path = vim.fs.joinpath(root, "CMakePresets.json")
  local ok, lines = pcall(vim.fn.readfile, path)
  if ok then
    local decoded_ok, data = pcall(vim.json.decode, table.concat(lines, "\n"))
    if decoded_ok and type(data) == "table" then
      for _, item in ipairs(data.configurePresets or {}) do
        if item.name == preset and item.binaryDir then return expand_cmake_path(item.binaryDir, root) end
      end
    end
  end

  return vim.fs.joinpath(root, "build", preset)
end

local function cmake_project_name(root)
  local path = vim.fs.joinpath(root, "CMakeLists.txt")
  local ok, lines = pcall(vim.fn.readfile, path)
  if not ok then return vim.fn.fnamemodify(root, ":t") end

  local in_project = false
  for _, line in ipairs(lines) do
    local inline_name = line:match "project%s*%(%s*([%w_%.%-]+)"
    if inline_name then return inline_name end

    if in_project then
      local name = line:match "^%s*([%w_%.%-]+)"
      if name then return name end
    elseif line:match "project%s*%(" then
      in_project = true
    end
  end

  return vim.fn.fnamemodify(root, ":t")
end

local function cmake_build_or_run_command(root, preset, run_after_build)
  local cmake = first_executable { "cmake" }
  if not cmake then return nil, "No `cmake` executable found on PATH." end

  local pieces = {
    shell_executable(cmake),
    "--build",
    "--preset",
    shell_quote(preset),
  }

  local script = table.concat(pieces, " ")
  if run_after_build then
    local output =
      vim.fs.joinpath(cmake_preset_binary_dir(root, preset), cmake_project_name(root) .. executable_suffix())
    if is_windows() then
      script = script .. "; if ($LASTEXITCODE -eq 0) { & " .. powershell_quote(output) .. " }"
    else
      script = script .. " && " .. shell_quote(output)
    end
  end

  return shell_command(script, root, run_after_build and " CMake run " or " CMake build ")
end

local function make_root(bufnr)
  return vim.fs.root(bufnr, {
    "GNUmakefile",
    "makefile",
    "Makefile",
  })
end

local function makefile_path(root)
  if not root then return end
  for _, name in ipairs { "GNUmakefile", "makefile", "Makefile" } do
    local path = vim.fs.joinpath(root, name)
    if vim.uv.fs_stat(path) then return path end
  end
end

local function make_targets(root)
  local targets = {}
  local path = makefile_path(root)
  if not path then return targets end

  local ok, lines = pcall(vim.fn.readfile, path)
  if not ok then return targets end

  for _, line in ipairs(lines) do
    if not line:match "^%s*#" and not line:match "^%s*[A-Za-z0-9_%.%-]+%s*[%+%?%!:]?=" then
      local target = line:match "^([A-Za-z0-9_%.%-]+)%s*:"
      if target and not target:match "^%." then targets[target] = true end
    end
  end

  return targets
end

local function preferred_make_target(root, names)
  local targets = make_targets(root)
  for _, name in ipairs(names) do
    if targets[name] then return name end
  end
end

local function make_action_command(bufnr, action)
  local root = make_root(bufnr)
  if not root then return nil, nil, nil end

  local make = first_executable { "make", "mingw32-make", "gmake" }
  if not make then return nil, "No `make` executable found on PATH.", root end

  local target
  if action == "run" then
    target = preferred_make_target(root, { "run" })
    if not target then return nil, "No `run` target found in Makefile. Add `run:` or use `,cb` to build only.", root end
  elseif action == "clean" then
    target = preferred_make_target(root, { "clean", "distclean", "mrproper" })
    if not target then return nil, "No clean target found in Makefile. Expected `clean:`.", root end
  elseif action == "test" then
    target = preferred_make_target(root, { "test", "check" })
    if not target then return nil, "No `test` or `check` target found in Makefile.", root end
  end

  local pieces = { shell_executable(make) }
  if target then pieces[#pieces + 1] = shell_quote(target) end

  return shell_command(table.concat(pieces, " "), root, " Make "), nil, root
end

local function cpp_mode_label(bufnr)
  if cmake_root(bufnr) then return "CMake project" end
  if make_root(bufnr) then return "Make project" end
  return "Single file"
end

local function single_file_command(bufnr, run_after_build)
  local file = vim.fn.fnamemodify(vim.api.nvim_buf_get_name(bufnr), ":p")
  local dir = vim.fn.fnamemodify(file, ":h")
  local stem = vim.fn.fnamemodify(file, ":t:r")
  local output = vim.fs.joinpath(dir, stem .. executable_suffix())
  local filetype = vim.bo[bufnr].filetype

  local compiler
  local args

  if filetype == "c" then
    compiler = first_executable { "clang", "gcc" }
    args = {
      "-std=c17",
      "-Wall",
      "-Wextra",
      "-g",
      file,
      "-o",
      output,
    }
  elseif vim.tbl_contains({ "cpp", "objc", "objcpp" }, filetype) then
    compiler = first_executable { "clang++", "g++" }
    args = {
      "-std=c++23",
      "-Wall",
      "-Wextra",
      "-g",
      file,
      "-o",
      output,
    }
  else
    return nil, "Single-file build is only configured for C/C++ source files."
  end

  if not compiler then return nil, "No compiler found on PATH. Expected clang/clang++ or gcc/g++." end

  local pieces = { shell_executable(compiler) }
  for _, arg in ipairs(args) do
    pieces[#pieces + 1] = shell_quote(arg)
  end

  local script = table.concat(pieces, " ")
  if run_after_build then
    if is_windows() then
      script = script .. "; if ($LASTEXITCODE -eq 0) { & " .. powershell_quote(output) .. " }"
    else
      script = script .. " && " .. shell_quote(output)
    end
  end

  return shell_command(
    script,
    dir,
    run_after_build and (" run: " .. vim.fn.fnamemodify(file, ":t") .. " ")
      or (" build: " .. vim.fn.fnamemodify(file, ":t") .. " ")
  )
end

M.cmake_root = cmake_root
M.make_root = make_root
M.preferred_make_target = preferred_make_target
M.mode_label = cpp_mode_label

function M.command(bufnr, action)
  if not vim.list_contains({ "build", "run", "test", "clean" }, action) then
    return nil, "C/C++ does not support action: " .. tostring(action)
  end
  local root = cmake_root(bufnr)
  if root then
    local commands = { build = "CMakeBuild", run = "CMakeRun", test = "CMakeRunTest", clean = "CMakeClean" }
    local command = commands[action]
    if command and vim.fn.exists(":" .. command) == 2 then
      return {
        buffer = bufnr,
        execute = function()
          vim.api.nvim_buf_call(bufnr, function() vim.cmd(command) end)
        end,
      }
    end
    if action == "build" then
      local config = require("user").lang.cpp or {}
      return cmake_build_or_run_command(root, config.cmake_preset or "default-debug", false)
    end
    return nil, "Load cmake-tools for target selection, running, cleaning and CTest."
  end
  local result, err, make_dir = make_action_command(bufnr, action)
  if make_dir then return result, err end
  if action == "build" or action == "run" then return single_file_command(bufnr, action == "run") end
  if action == "clean" then
    local file = vim.api.nvim_buf_get_name(bufnr)
    return {
      clean = true,
      paths = { vim.fs.joinpath(vim.fs.dirname(file), vim.fn.fnamemodify(file, ":t:r") .. executable_suffix()) },
      title = "C/C++",
    }
  end
  return nil, "No test target is configured for single-file C/C++."
end

return M
