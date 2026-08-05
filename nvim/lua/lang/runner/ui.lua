local M = {}

local detect = require "lang.runner.detect"
local frontend_filetypes = require "lang.frontend_filetypes"

function M.backend_mode(bufnr, filetype)
  if filetype == "rust" then
    local root = detect.is_rust_project(bufnr)
    if root and detect.file_exists(vim.fs.joinpath(root, "Cargo.toml")) then return "Cargo project" end
    return "Single file"
  elseif filetype == "zig" then
    local root = detect.is_zig_project(bufnr)
    if root and detect.file_exists(vim.fs.joinpath(root, "build.zig")) then return "Zig project" end
    return "Single file"
  elseif filetype == "go" then
    local root = detect.is_go_project(bufnr)
    if root and vim.fn.filereadable(vim.fs.joinpath(root, "go.mod")) == 1 then return "Go module" end
    return "Single file"
  elseif filetype == "python" then
    local root = detect.is_python_project(bufnr)
    if root and detect.file_exists(vim.fs.joinpath(root, "uv.lock")) then
      return "uv project"
    elseif detect.find_local_python(root or vim.fn.fnamemodify(vim.api.nvim_buf_get_name(bufnr), ":h")) then
      return "Virtualenv"
    end
    return "Single file"
  elseif vim.tbl_contains(frontend_filetypes.lsp_filetypes, filetype) then
    local root = detect.package_json_root(bufnr)
    if not root then return "No package.json" end
    return (detect.detect_package_manager(root) or "Package") .. " project"
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
      { key = "rl", desc = "Repeat last run", action = "repeat_last" },
      { key = "rc", desc = "Clean", action = "clean" },
    }
  elseif filetype == "python" then
    return {
      { key = "rk", desc = "Show runner keymaps" },
      { key = "rb", desc = "Compile check", action = "build" },
      { key = "rr", desc = "Run", action = "run" },
      { key = "rt", desc = "Test", action = "test" },
      { key = "rT", desc = "Test current file", action = "test_file" },
      { key = "rl", desc = "Repeat last run", action = "repeat_last" },
      { key = "rc", desc = "Clean", action = "clean" },
    }
  elseif vim.tbl_contains(frontend_filetypes.lsp_filetypes, filetype) then
    return {
      { key = "rk", desc = "Show runner keymaps" },
      { key = "rd", desc = "Dev", action = "dev" },
      { key = "rb", desc = "Build", action = "build" },
      { key = "rt", desc = "Test", action = "test" },
      { key = "rT", desc = "Test current file", action = "test_file" },
      { key = "rl", desc = "Repeat last run", action = "repeat_last" },
      { key = "rc", desc = "Clean", action = "clean" },
    }
  end

  return {}
end

function M.supports(filetype) return not vim.tbl_isempty(runner_entries(filetype)) end

local function runner_group_label(filetype)
  if vim.tbl_contains(frontend_filetypes.lsp_filetypes, filetype) then
    return "Frontend Runner"
  elseif filetype == "python" then
    return "Python Runner"
  end
  return filetype:upper() .. " Runner"
end

local function runner_title(filetype)
  if vim.tbl_contains(frontend_filetypes.lsp_filetypes, filetype) then
    return "Frontend keymaps"
  elseif filetype == "python" then
    return "Python keymaps"
  end
  return filetype:upper() .. " keymaps"
end

local function key_lines(bufnr, filetype)
  local file = vim.api.nvim_buf_get_name(bufnr)
  local lines = {
    runner_title(filetype),
    "",
    "Buffer: " .. (file ~= "" and file or "[No Name]"),
    "Mode:   " .. M.backend_mode(bufnr, filetype),
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

  if filetype == "python" or vim.tbl_contains(frontend_filetypes.lsp_filetypes, filetype) then
    lines[#lines + 1] = ""
    lines[#lines + 1] = "File test"
    lines[#lines + 1] = ",rT  Test current file"
  end

  return lines
end

function M.show_keys(bufnr, filetype)
  vim.notify(table.concat(key_lines(bufnr, filetype), "\n"), vim.log.levels.INFO, {
    title = filetype:upper() .. " Keys",
  })
end

function M.register_which_key(bufnr, filetype)
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

function M.entries(filetype) return runner_entries(filetype) end

return M
