local M = {}

local function is_windows()
  return vim.fn.has("win32") == 1 or vim.fn.has("win64") == 1
end

local function executable_suffix()
  return is_windows() and ".exe" or ""
end

local function powershell_quote(value)
  return "'" .. tostring(value):gsub("'", "''") .. "'"
end

local function shell_quote(value)
  if is_windows() then return powershell_quote(value) end
  return "'" .. tostring(value):gsub("'", [['"'"']]) .. "'"
end

local function first_executable(names)
  for _, name in ipairs(names) do
    local path = vim.fn.exepath(name)
    if path ~= nil and path ~= "" then return path end
  end
end

local function file_exists(path)
  return path and vim.uv.fs_stat(path) ~= nil
end

local function is_rust_project(bufnr)
  return vim.fs.root(bufnr, { "Cargo.toml", "rust-project.json", ".git" })
end

local function is_go_project(bufnr)
  return vim.fs.root(bufnr, { "go.work", "go.mod", ".git" })
end

local function is_zig_project(bufnr)
  return vim.fs.root(bufnr, { "build.zig", "zls.json", ".git" })
end

local function is_python_project(bufnr)
  return vim.fs.root(bufnr, { "pyproject.toml", "uv.lock", "requirements.txt", ".git" })
end

local function package_json_root(bufnr)
  return vim.fs.root(bufnr, { "package.json" })
end

local function ensure_file_is_saved(bufnr, title)
  if vim.api.nvim_buf_get_name(bufnr) == "" then
    vim.notify("Current buffer has no file path yet.", vim.log.levels.WARN, { title = title })
    return false
  end

  if vim.bo[bufnr].modified then
    local ok, err = pcall(vim.cmd.write)
    if not ok then
      vim.notify("Failed to save file before running command: " .. tostring(err), vim.log.levels.ERROR, { title = title })
      return false
    end
  end

  return true
end

local function open_command(cmd, opts)
  require("core.terminal").open_float(cmd, opts)
end

local function shell_command(command, cwd, title)
  if is_windows() then
    return {
      cmd = {
        "powershell.exe",
        "-NoLogo",
        "-NoProfile",
        "-ExecutionPolicy",
        "Bypass",
        "-Command",
        command,
      },
      opts = {
        cwd = cwd,
        title = title,
      },
    }
  end

  local shell = vim.o.shell ~= "" and vim.o.shell or "sh"
  local shellcmdflag = vim.o.shellcmdflag ~= "" and vim.o.shellcmdflag or "-c"
  return {
    cmd = {
      shell,
      shellcmdflag,
      command,
    },
    opts = {
      cwd = cwd,
      title = title,
    },
  }
end

local function find_upward(start_dir, names)
  if not start_dir or start_dir == "" then return end
  local dir = vim.fs.normalize(start_dir)
  while dir and dir ~= "" do
    for _, name in ipairs(names) do
      local path = vim.fs.joinpath(dir, name)
      if file_exists(path) then return path, dir end
    end
    local parent = vim.fs.dirname(dir)
    if not parent or parent == dir then break end
    dir = parent
  end
end

local function find_local_python(start_dir)
  local path = select(1, find_upward(start_dir, {
    is_windows() and ".venv\\Scripts\\python.exe" or ".venv/bin/python",
    is_windows() and "venv\\Scripts\\python.exe" or "venv/bin/python",
  }))
  return path
end

local function package_json_data(root)
  if not root then return end
  local path = vim.fs.joinpath(root, "package.json")
  if not file_exists(path) then return end

  local ok_read, lines = pcall(vim.fn.readfile, path)
  if not ok_read then return end

  local ok_decode, data = pcall(vim.json.decode, table.concat(lines, "\n"))
  if ok_decode and type(data) == "table" then return data end
end

local function detect_package_manager(root)
  local lock_path = select(1, find_upward(root, {
    "pnpm-lock.yaml",
    "bun.lockb",
    "bun.lock",
    "yarn.lock",
    "package-lock.json",
  }))

  if lock_path then
    local name = vim.fs.basename(lock_path)
    if name == "pnpm-lock.yaml" and vim.fn.executable("pnpm") == 1 then
      return "pnpm"
    elseif (name == "bun.lockb" or name == "bun.lock") and vim.fn.executable("bun") == 1 then
      return "bun"
    elseif name == "yarn.lock" and vim.fn.executable("yarn") == 1 then
      return "yarn"
    elseif name == "package-lock.json" and vim.fn.executable("npm") == 1 then
      return "npm"
    end
  end

  if vim.fn.executable("pnpm") == 1 then
    return "pnpm"
  elseif vim.fn.executable("bun") == 1 then
    return "bun"
  elseif vim.fn.executable("yarn") == 1 then
    return "yarn"
  elseif vim.fn.executable("npm") == 1 then
    return "npm"
  end
end

local function package_manager_command(manager, script)
  if manager == "bun" then
    return "bun run " .. script
  elseif manager == "yarn" then
    return "yarn run " .. script
  elseif manager == "pnpm" then
    return "pnpm run " .. script
  end
  return "npm run " .. script
end

local function remove_files(paths, title)
  local removed = {}
  for _, path in ipairs(paths) do
    if file_exists(path) then
      local kind = vim.fn.isdirectory(path) == 1 and "rf" or nil
      local ok, err = pcall(vim.fn.delete, path, kind)
      if not ok or file_exists(path) then
        vim.notify("Failed to remove " .. path .. ": " .. tostring(err), vim.log.levels.ERROR, { title = title })
        return
      end
      table.insert(removed, path)
    end
  end

  if #removed > 0 then
    vim.notify("Removed " .. table.concat(removed, ", "), vim.log.levels.INFO, { title = title })
  else
    vim.notify("Nothing to clean.", vim.log.levels.INFO, { title = title })
  end
end

local function rust_single_file_command(bufnr, action)
  local file = vim.fn.fnamemodify(vim.api.nvim_buf_get_name(bufnr), ":p")
  local dir = vim.fn.fnamemodify(file, ":h")
  local stem = vim.fn.fnamemodify(file, ":t:r")
  local output = vim.fs.joinpath(dir, stem .. executable_suffix())
  local test_output = vim.fs.joinpath(dir, stem .. "_test" .. executable_suffix())
  local rustc = first_executable({ "rustc" })
  if not rustc then
    return nil, "No `rustc` executable found on PATH."
  end

  if action == "clean" then
    return {
      clean = true,
      paths = { output, test_output },
      title = "RUST",
    }
  end

  local pieces = { shell_quote(rustc) }
  if action == "build" then
    pieces[#pieces + 1] = shell_quote(file)
    pieces[#pieces + 1] = "-o"
    pieces[#pieces + 1] = shell_quote(output)
  elseif action == "run" then
    pieces[#pieces + 1] = shell_quote(file)
    pieces[#pieces + 1] = "-o"
    pieces[#pieces + 1] = shell_quote(output)
    if is_windows() then
      pieces[#pieces + 1] = ";"
      pieces[#pieces + 1] = "if ($LASTEXITCODE -eq 0) { & " .. powershell_quote(output) .. " }"
    else
      pieces[#pieces + 1] = "&&"
      pieces[#pieces + 1] = shell_quote(output)
    end
  elseif action == "test" then
    pieces[#pieces + 1] = "--test"
    pieces[#pieces + 1] = shell_quote(file)
    pieces[#pieces + 1] = "-o"
    pieces[#pieces + 1] = shell_quote(test_output)
    if is_windows() then
      pieces[#pieces + 1] = ";"
      pieces[#pieces + 1] = "if ($LASTEXITCODE -eq 0) { & " .. powershell_quote(test_output) .. " }"
    else
      pieces[#pieces + 1] = "&&"
      pieces[#pieces + 1] = shell_quote(test_output)
    end
  end

  return shell_command(table.concat(pieces, " "), dir, " Rust ")
end

local function rust_command(bufnr, action)
  local root = is_rust_project(bufnr)
  if root and file_exists(vim.fs.joinpath(root, "Cargo.toml")) then
    local title = " Rust "
    if action == "build" then
      return shell_command("cargo build", root, title)
    elseif action == "run" then
      return shell_command("cargo run", root, title)
    elseif action == "test" then
      return shell_command("cargo test", root, title)
    elseif action == "clean" then
      return shell_command("cargo clean", root, title)
    end
  end

  return rust_single_file_command(bufnr, action)
end

local function zig_single_file_command(bufnr, action)
  local file = vim.fn.fnamemodify(vim.api.nvim_buf_get_name(bufnr), ":p")
  local dir = vim.fn.fnamemodify(file, ":h")
  local stem = vim.fn.fnamemodify(file, ":t:r")
  local output = vim.fs.joinpath(dir, stem .. executable_suffix())
  local zig = first_executable({ "zig" })
  if not zig then
    return nil, "No `zig` executable found on PATH."
  end

  if action == "clean" then
    return {
      clean = true,
      paths = { output },
      title = "ZIG",
    }
  end

  local pieces = { shell_quote(zig) }
  if action == "build" then
    pieces[#pieces + 1] = "build-exe"
    pieces[#pieces + 1] = shell_quote(file)
  elseif action == "run" then
    pieces[#pieces + 1] = "run"
    pieces[#pieces + 1] = shell_quote(file)
  elseif action == "test" then
    pieces[#pieces + 1] = "test"
    pieces[#pieces + 1] = shell_quote(file)
  else
    return nil, "Single-file Zig clean is not configured."
  end

  return shell_command(table.concat(pieces, " "), dir, " Zig ")
end

local function zig_command(bufnr, action)
  local root = is_zig_project(bufnr)
  if root and vim.fn.filereadable(vim.fs.joinpath(root, "build.zig")) == 1 then
    if action == "build" then
      return shell_command("zig build", root, " Zig ")
    elseif action == "run" then
      return shell_command("zig build run", root, " Zig ")
    elseif action == "test" then
      return shell_command("zig build test", root, " Zig ")
    elseif action == "clean" then
      return nil, "Zig project clean is not configured. `zig build` has no universal clean command."
    end
  end

  return zig_single_file_command(bufnr, action)
end

local function go_command(bufnr, action)
  local root = is_go_project(bufnr)
  local file = vim.fn.fnamemodify(vim.api.nvim_buf_get_name(bufnr), ":p")
  local dir = vim.fn.fnamemodify(file, ":h")

  if action == "build" then
    if root and vim.fn.filereadable(vim.fs.joinpath(root, "go.mod")) == 1 then
      return shell_command("go build .", dir, " Go ")
    end
    return shell_command("go build " .. shell_quote(file), dir, " Go ")
  elseif action == "run" then
    if root and vim.fn.filereadable(vim.fs.joinpath(root, "go.mod")) == 1 then
      return shell_command("go run .", dir, " Go ")
    end
    return shell_command("go run " .. shell_quote(file), dir, " Go ")
  elseif action == "test" then
    return shell_command("go test", dir, " Go ")
  elseif action == "clean" then
    if root and vim.fn.filereadable(vim.fs.joinpath(root, "go.mod")) == 1 then
      return shell_command("go clean", dir, " Go ")
    end
    return nil, "Single-file Go clean is not configured."
  end
end

local function python_command(bufnr, action)
  local root = is_python_project(bufnr)
  local file = vim.fn.fnamemodify(vim.api.nvim_buf_get_name(bufnr), ":p")
  local dir = vim.fn.fnamemodify(file, ":h")
  local cwd = root or dir
  local uv_lock = root and file_exists(vim.fs.joinpath(root, "uv.lock"))
  local local_python = find_local_python(root or dir)
  local python = local_python or first_executable({ "python", "python3" })
  local cache_dir = vim.fs.joinpath(dir, "__pycache__")

  if action == "clean" then
    return {
      clean = true,
      paths = { cache_dir },
      title = "PYTHON",
    }
  end

  if uv_lock and vim.fn.executable("uv") == 1 then
    if action == "build" then
      return shell_command("uv run python -m py_compile " .. shell_quote(file), cwd, " Python ")
    elseif action == "run" then
      return shell_command("uv run python " .. shell_quote(file), cwd, " Python ")
    elseif action == "test" then
      return shell_command("uv run pytest", cwd, " Python ")
    end
  end

  if not python then
    return nil, "No Python executable found on PATH."
  end

  local quoted_python = shell_quote(python)
  if action == "build" then
    return shell_command(quoted_python .. " -m py_compile " .. shell_quote(file), cwd, " Python ")
  elseif action == "run" then
    return shell_command(quoted_python .. " " .. shell_quote(file), cwd, " Python ")
  elseif action == "test" then
    return shell_command(quoted_python .. " -m pytest", cwd, " Python ")
  end
end

local function frontend_command(bufnr, action)
  local root = package_json_root(bufnr)
  if not root then
    return nil, "Frontend runner requires a nearby package.json."
  end

  local data = package_json_data(root)
  local scripts = data and data.scripts or {}
  if action == "clean" and (type(scripts) ~= "table" or type(scripts.clean) ~= "string" or scripts.clean == "") then
    return nil, "No `clean` script found in package.json."
  end
  if type(scripts) ~= "table" or type(scripts[action]) ~= "string" or scripts[action] == "" then
    return nil, ("No `%s` script found in package.json."):format(action)
  end

  local manager = detect_package_manager(root)
  if not manager then
    return nil, "No supported package manager found on PATH. Expected pnpm, bun, yarn, or npm."
  end

  return shell_command(package_manager_command(manager, action), root, " Frontend ")
end

local function backend_command(bufnr, filetype, action)
  if filetype == "rust" then
    return rust_command(bufnr, action)
  elseif filetype == "zig" then
    return zig_command(bufnr, action)
  elseif filetype == "go" then
    return go_command(bufnr, action)
  elseif filetype == "python" then
    return python_command(bufnr, action)
  elseif vim.tbl_contains({ "javascript", "javascriptreact", "typescript", "typescriptreact", "vue" }, filetype) then
    return frontend_command(bufnr, action)
  end
end

local function backend_mode(bufnr, filetype)
  if filetype == "rust" then
    local root = is_rust_project(bufnr)
    if root and file_exists(vim.fs.joinpath(root, "Cargo.toml")) then
      return "Cargo project"
    end
    return "Single file"
  elseif filetype == "zig" then
    local root = is_zig_project(bufnr)
    if root and file_exists(vim.fs.joinpath(root, "build.zig")) then
      return "Zig project"
    end
    return "Single file"
  elseif filetype == "go" then
    local root = is_go_project(bufnr)
    if root and vim.fn.filereadable(vim.fs.joinpath(root, "go.mod")) == 1 then
      return "Go module"
    end
    return "Single file"
  elseif filetype == "python" then
    local root = is_python_project(bufnr)
    if root and file_exists(vim.fs.joinpath(root, "uv.lock")) then
      return "uv project"
    elseif find_local_python(root or vim.fn.fnamemodify(vim.api.nvim_buf_get_name(bufnr), ":h")) then
      return "Virtualenv"
    end
    return "Single file"
  elseif vim.tbl_contains({ "javascript", "javascriptreact", "typescript", "typescriptreact", "vue" }, filetype) then
    local root = package_json_root(bufnr)
    if not root then return "No package.json" end
    return (detect_package_manager(root) or "Package") .. " project"
  end
  return "Unknown"
end

local function runner_entries(filetype)
  if vim.tbl_contains({ "rust", "zig", "go" }, filetype) then
    return {
      { key = "rk", desc = "Show runner keymaps" },
      { key = "rb", desc = "Build", action = "build" },
      { key = "rr", desc = "Run", action = "run" },
      { key = "rt", desc = "Test", action = "test" },
      { key = "rc", desc = "Clean", action = "clean" },
    }
  elseif filetype == "python" then
    return {
      { key = "rk", desc = "Show runner keymaps" },
      { key = "rb", desc = "Compile check", action = "build" },
      { key = "rr", desc = "Run", action = "run" },
      { key = "rt", desc = "Test", action = "test" },
      { key = "rc", desc = "Clean", action = "clean" },
    }
  elseif vim.tbl_contains({ "javascript", "javascriptreact", "typescript", "typescriptreact", "vue" }, filetype) then
    return {
      { key = "rk", desc = "Show runner keymaps" },
      { key = "rd", desc = "Dev", action = "dev" },
      { key = "rb", desc = "Build", action = "build" },
      { key = "rt", desc = "Test", action = "test" },
      { key = "rc", desc = "Clean", action = "clean" },
    }
  end

  return {}
end

local function runner_group_label(filetype)
  if vim.tbl_contains({ "javascript", "javascriptreact", "typescript", "typescriptreact", "vue" }, filetype) then
    return "Frontend Runner"
  elseif filetype == "python" then
    return "Python Runner"
  end
  return filetype:upper() .. " Runner"
end

local function runner_title(filetype)
  if vim.tbl_contains({ "javascript", "javascriptreact", "typescript", "typescriptreact", "vue" }, filetype) then
    return "Frontend keymaps"
  elseif filetype == "python" then
    return "Python keymaps"
  end
  return filetype:upper() .. " keymaps"
end

local function backend_key_lines(bufnr, filetype)
  local file = vim.api.nvim_buf_get_name(bufnr)
  local lines = {
    runner_title(filetype),
    "",
    "Buffer: " .. (file ~= "" and file or "[No Name]"),
    "Mode:   " .. backend_mode(bufnr, filetype),
    "",
  }

  for _, entry in ipairs(runner_entries(filetype)) do
    lines[#lines + 1] = ("," .. entry.key .. "  " .. entry.desc)
  end

  if filetype == "go" then
    lines[#lines + 1] = ""
    lines[#lines + 1] = "Go extras"
    lines[#lines + 1] = ",aT  Remove tags"
    lines[#lines + 1] = ",tf  Test function"
    lines[#lines + 1] = ",ta  Test package"
    lines[#lines + 1] = ",tc  Test coverage"
  end

  return lines
end

local function show_backend_keys(bufnr, filetype)
  vim.notify(table.concat(backend_key_lines(bufnr, filetype), "\n"), vim.log.levels.INFO, {
    title = filetype:upper() .. " Keys",
  })
end

local function register_backend_which_key(bufnr, filetype)
  local ok, which_key = pcall(require, "which-key")
  if not ok then return end

  local spec = {
    { "<localleader>r", group = runner_group_label(filetype) },
  }
  for _, entry in ipairs(runner_entries(filetype)) do
    spec[#spec + 1] = { "<localleader>" .. entry.key, desc = entry.desc }
  end
  which_key.add(spec, { buffer = bufnr })
end

local function run_backend_action(bufnr, filetype, action)
  if not ensure_file_is_saved(bufnr, filetype:upper()) then return end

  local result, err = backend_command(bufnr, filetype, action)
  if not result then
    vim.notify(err or ("No runner is configured for " .. filetype .. " -> " .. action), vim.log.levels.INFO, {
      title = filetype:upper(),
    })
    return
  end

  if result.clean then
    remove_files(result.paths or {}, result.title or filetype:upper())
    return
  end

  open_command(result.cmd, result.opts)
end

function M.supports(filetype)
  return not vim.tbl_isempty(runner_entries(filetype))
end

function M.show_keys(bufnr)
  bufnr = bufnr or vim.api.nvim_get_current_buf()
  local filetype = vim.bo[bufnr].filetype
  if not M.supports(filetype) then
    vim.notify("RunnerKeys currently supports rust, zig, go, python, javascript, typescript, and vue buffers.", vim.log.levels.INFO, {
      title = "RunnerKeys",
    })
    return
  end
  show_backend_keys(bufnr, filetype)
end

function M.setup()
  local map = require("core.keymaps").map

  vim.api.nvim_create_user_command("RunnerKeys", function()
    M.show_keys(vim.api.nvim_get_current_buf())
  end, { desc = "Show backend runner keymaps" })

  vim.api.nvim_create_autocmd("FileType", {
    group = vim.api.nvim_create_augroup("lang_runner_keymaps", { clear = true }),
    pattern = { "rust", "zig", "go", "python", "javascript", "javascriptreact", "typescript", "typescriptreact", "vue" },
    callback = function(event)
      local filetype = vim.bo[event.buf].filetype
      register_backend_which_key(event.buf, filetype)
      for _, entry in ipairs(runner_entries(filetype)) do
        if entry.key == "rk" then
          map("n", "<localleader>rk", function() show_backend_keys(event.buf, filetype) end, entry.desc, { buffer = event.buf })
        else
          map("n", "<localleader>" .. entry.key, function() run_backend_action(event.buf, filetype, entry.action) end, entry.desc, { buffer = event.buf })
        end
      end
    end,
  })
end

return M
