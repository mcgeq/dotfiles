local M = {
  spec = {
    { src = "https://github.com/p00f/clangd_extensions.nvim", name = "clangd_extensions.nvim" },
    { src = "https://github.com/Civitasv/cmake-tools.nvim", name = "cmake-tools.nvim" },
  },
}

local util = require("core.util")
local is_windows = util.is_windows
local executable_suffix = util.executable_suffix
local shell_quote = util.shell_quote

local function is_usable_executable_path(path)
  if type(path) ~= "string" or path == "" then return false end
  if not is_windows() then return true end

  local lower = path:lower()
  return path:match("^[A-Za-z]:[\\/]")
    or path:match("^\\\\")
    or lower:match("%.exe$")
    or lower:match("%.cmd$")
    or lower:match("%.bat$")
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

local function shell_command(command, cwd, title)
  if is_windows() then
    return {
      "powershell.exe",
      "-NoLogo",
      "-NoProfile",
      "-ExecutionPolicy",
      "Bypass",
      "-Command",
      command,
    }, {
      cwd = cwd,
      title = title,
    }
  end

  local shell = vim.o.shell ~= "" and vim.o.shell or "sh"
  local shellcmdflag = vim.o.shellcmdflag ~= "" and vim.o.shellcmdflag or "-c"
  return {
    shell,
    shellcmdflag,
    command,
  }, {
    cwd = cwd,
    title = title,
  }
end

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
        if item.name == preset and item.binaryDir then
          return expand_cmake_path(item.binaryDir, root)
        end
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
    local inline_name = line:match("project%s*%(%s*([%w_%.%-]+)")
    if inline_name then return inline_name end

    if in_project then
      local name = line:match("^%s*([%w_%.%-]+)")
      if name then return name end
    elseif line:match("project%s*%(") then
      in_project = true
    end
  end

  return vim.fn.fnamemodify(root, ":t")
end

local function cmake_build_or_run_command(root, preset, run_after_build)
  local cmake = first_executable({ "cmake" })
  if not cmake then return nil, "No `cmake` executable found on PATH." end

  local pieces = {
    shell_executable(cmake),
    "--build",
    "--preset",
    shell_quote(preset),
  }

  local script = table.concat(pieces, " ")
  if run_after_build then
    local output = vim.fs.joinpath(cmake_preset_binary_dir(root, preset), cmake_project_name(root) .. executable_suffix())
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
  for _, name in ipairs({ "GNUmakefile", "makefile", "Makefile" }) do
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
    if not line:match("^%s*#") and not line:match("^%s*[A-Za-z0-9_%.%-]+%s*[%+%?%!:]?=") then
      local target = line:match("^([A-Za-z0-9_%.%-]+)%s*:")
      if target and not target:match("^%.") then
        targets[target] = true
      end
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

  local make = first_executable({ "make", "mingw32-make", "gmake" })
  if not make then
    return nil, "No `make` executable found on PATH.", root
  end

  local target
  if action == "run" then
    target = preferred_make_target(root, { "run" })
    if not target then
      return nil, "No `run` target found in Makefile. Add `run:` or use `,cb` to build only.", root
    end
  elseif action == "clean" then
    target = preferred_make_target(root, { "clean", "distclean", "mrproper" })
    if not target then
      return nil, "No clean target found in Makefile. Expected `clean:`.", root
    end
  elseif action == "test" then
    target = preferred_make_target(root, { "test", "check" })
    if not target then
      return nil, "No `test` or `check` target found in Makefile.", root
    end
  end

  local pieces = { shell_quote(make) }
  if target then
    pieces[#pieces + 1] = shell_quote(target)
  end

  return shell_command(table.concat(pieces, " "), root, " Make "), root
end

local function cpp_mode_label(bufnr)
  if cmake_root(bufnr) then return "CMake project" end
  if make_root(bufnr) then return "Make project" end
  return "Single file"
end

local function ensure_file_is_saved(bufnr)
  if vim.api.nvim_buf_get_name(bufnr) == "" then
    vim.notify("Current buffer has no file path yet.", vim.log.levels.WARN, { title = "C/C++" })
    return false
  end

  if vim.bo[bufnr].modified then
    local ok, err = pcall(vim.cmd.write)
    if not ok then
      vim.notify("Failed to save file before build: " .. tostring(err), vim.log.levels.ERROR, { title = "C/C++" })
      return false
    end
  end

  return true
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
    compiler = first_executable({ "clang", "gcc" })
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
    compiler = first_executable({ "clang++", "g++" })
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

  if not compiler then
    return nil, "No compiler found on PATH. Expected clang/clang++ or gcc/g++."
  end

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
    run_after_build and (" run: " .. vim.fn.fnamemodify(file, ":t") .. " ") or (" build: " .. vim.fn.fnamemodify(file, ":t") .. " ")
  )
end

local function build_or_run_file(bufnr, run_after_build)
  if not ensure_file_is_saved(bufnr) then return end

  local cmake_command = run_after_build and "CMakeRun" or "CMakeBuild"
  if cmake_root(bufnr) and vim.fn.exists(":" .. cmake_command) == 2 then
    vim.cmd(cmake_command)
    return
  end

  local terminal = require("core.terminal")
  local cmake_project_root = cmake_root(bufnr)
  if cmake_project_root then
    local command, opts_or_error = cmake_build_or_run_command(cmake_project_root, "default-debug", run_after_build)
    if not command then
      vim.notify(opts_or_error, vim.log.levels.WARN, { title = "C/C++" })
      return
    end

    terminal.open_float(command, opts_or_error)
    return
  end

  local make_command, make_opts_or_error, make_project_root = make_action_command(bufnr, run_after_build and "run" or "build")
  if make_project_root then
    if not make_command then
      vim.notify(make_opts_or_error, vim.log.levels.WARN, { title = "C/C++" })
      return
    end

    terminal.open_float(make_command, make_opts_or_error)
    return
  end

  local command, opts_or_error = single_file_command(bufnr, run_after_build)
  if not command then
    vim.notify(opts_or_error, vim.log.levels.WARN, { title = "C/C++" })
    return
  end

  terminal.open_float(command, opts_or_error)
end

local function clean_file_or_target(bufnr)
  if cmake_root(bufnr) and vim.fn.exists(":CMakeClean") == 2 then
    vim.cmd("CMakeClean")
    return
  end

  local terminal = require("core.terminal")
  local make_command, make_opts_or_error, make_project_root = make_action_command(bufnr, "clean")
  if make_project_root then
    if not make_command then
      vim.notify(make_opts_or_error, vim.log.levels.WARN, { title = "C/C++" })
      return
    end

    terminal.open_float(make_command, make_opts_or_error)
    return
  end

  local file = vim.fn.fnamemodify(vim.api.nvim_buf_get_name(bufnr), ":p")
  if file == "" then
    vim.notify("Current buffer has no file path yet.", vim.log.levels.WARN, { title = "C/C++" })
    return
  end

  local dir = vim.fn.fnamemodify(file, ":h")
  local stem = vim.fn.fnamemodify(file, ":t:r")
  local output = vim.fs.joinpath(dir, stem .. executable_suffix())

  if vim.uv.fs_stat(output) then
    local ok, err = pcall(vim.fn.delete, output)
    if ok and vim.uv.fs_stat(output) == nil then
      vim.notify("Removed " .. output, vim.log.levels.INFO, { title = "C/C++" })
    else
      vim.notify("Failed to remove " .. output .. ": " .. tostring(err), vim.log.levels.ERROR, { title = "C/C++" })
    end
  else
    vim.notify("No generated executable found: " .. output, vim.log.levels.INFO, { title = "C/C++" })
  end
end

local function run_tests(bufnr)
  if cmake_root(bufnr) and vim.fn.exists(":CMakeRunTest") == 2 then
    vim.cmd("CMakeRunTest")
    return
  end

  local terminal = require("core.terminal")
  local make_command, make_opts_or_error, make_project_root = make_action_command(bufnr, "test")
  if make_project_root then
    if not make_command then
      vim.notify(make_opts_or_error, vim.log.levels.INFO, { title = "C/C++" })
      return
    end

    terminal.open_float(make_command, make_opts_or_error)
    return
  end

  vim.notify("No test command is configured for single-file C/C++. Open a CMake project to use CTest.", vim.log.levels.INFO, {
    title = "C/C++",
  })
end

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
    lines[#lines + 1] = ",cr  " .. (preferred_make_target(make_project_root, { "run" }) and "Uses `make run`" or "Needs a `run:` target")
    lines[#lines + 1] = ",cc  " .. (preferred_make_target(make_project_root, { "clean", "distclean", "mrproper" }) and "Uses clean target" or "Needs a `clean:` target")
    lines[#lines + 1] = ",ct  " .. (preferred_make_target(make_project_root, { "test", "check" }) and "Uses `make test/check`" or "Needs a `test:` or `check:` target")
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

local function show_cpp_keys(bufnr)
  local lines = cpp_key_lines(bufnr or vim.api.nvim_get_current_buf())
  vim.notify(table.concat(lines, "\n"), vim.log.levels.INFO, { title = "CppKeys" })
end

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
  })
end

function M.setup()
  local map = require("core.keymaps").map

  setup_clangd_extensions()
  setup_cmake_tools()

  vim.api.nvim_create_user_command("CppKeys", function()
    show_cpp_keys(vim.api.nvim_get_current_buf())
  end, { desc = "Show C/C++ keymaps" })

  vim.api.nvim_create_autocmd("FileType", {
    group = vim.api.nvim_create_augroup("user_cpp_keymaps", { clear = true }),
    pattern = { "c", "cpp", "objc", "objcpp", "cuda" },
    callback = function(event)
      register_cpp_which_key(event.buf)
      map("n", "<localleader>ck", function() show_cpp_keys(event.buf) end, "Show C/C++ keymaps", { buffer = event.buf })
      map("n", "<localleader>cb", function() build_or_run_file(event.buf, false) end, "Build file or CMake target", { buffer = event.buf })
      map("n", "<localleader>cr", function() build_or_run_file(event.buf, true) end, "Run file or CMake target", { buffer = event.buf })
      map("n", "<localleader>cc", function() clean_file_or_target(event.buf) end, "Clean file or CMake target", { buffer = event.buf })
      map("n", "<localleader>ct", function() run_tests(event.buf) end, "Run tests", { buffer = event.buf })

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
        map("n", "<localleader>cT", "<cmd>ClangdTypeHierarchy<cr>", "Show type hierarchy", { buffer = event.buf })
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
      register_cmake_which_key(event.buf)
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
