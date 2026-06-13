local function normalize(path)
  if not path or path == "" then return nil end
  return vim.fs.normalize(path)
end

local function first_executable(names)
  for _, name in ipairs(names) do
    local path = normalize(vim.fn.exepath(name))
    if path then return path end
  end
end

local function executable_sibling(path, sibling)
  path = normalize(path)
  if not path then return nil end
  local parent = vim.fs.dirname(path)
  if not parent then return nil end
  local candidate = normalize(vim.fs.joinpath(parent, sibling))
  if candidate and vim.fn.executable(candidate) == 1 then return candidate end
end

local function system_clangd()
  local direct = first_executable({ "clangd", "clangd.exe" })
  if direct then return direct end

  local sibling = executable_sibling(vim.fn.exepath("clang++"), "clangd.exe")
    or executable_sibling(vim.fn.exepath("clang-cl"), "clangd.exe")
    or executable_sibling(vim.fn.exepath("clang"), "clangd.exe")
  if sibling then return sibling end
end

local function mason_clangd()
  local patterns = {
    vim.fs.joinpath(vim.fn.stdpath("data"), "mason", "packages", "clangd", "*", "bin", "clangd.exe"),
    vim.fs.joinpath(vim.fn.stdpath("data"), "mason", "packages", "clangd", "*", "bin", "clangd"),
  }

  for _, pattern in ipairs(patterns) do
    local matches = vim.fn.glob(pattern, false, true)
    local path = normalize(matches[1])
    if path then return path end
  end
end

local function unique(list)
  local seen = {}
  local result = {}
  for _, item in ipairs(list) do
    if item and not seen[item] then
      seen[item] = true
      table.insert(result, item)
    end
  end
  return result
end

local function build_cmd()
  local clangd = system_clangd() or mason_clangd() or "clangd"
  local drivers = unique({
    normalize(vim.fn.exepath("clang++")),
    normalize(vim.fn.exepath("clang-cl")),
    normalize(vim.fn.exepath("clang")),
    normalize(vim.fn.exepath("cl")),
  })

  local cmd = {
    clangd,
    "--background-index",
    "--clang-tidy",
    "--completion-style=detailed",
    "--header-insertion=iwyu",
    "--pch-storage=memory",
  }

  if #drivers > 0 then
    table.insert(cmd, "--query-driver=" .. table.concat(drivers, ","))
  end

  return cmd
end

return {
  name = "clangd",
  ensure_installed = true,
  config = {
    cmd = build_cmd(),
    capabilities = {
      offsetEncoding = { "utf-8" },
    },
    filetypes = { "c", "cpp", "objc", "objcpp", "cuda" },
    root_markers = {
      "compile_commands.json",
      "compile_flags.txt",
      ".clangd",
      "CMakeLists.txt",
      "meson.build",
      ".git",
    },
    init_options = {
      clangdFileStatus = true,
      fallbackFlags = { "-std=c++23", "-xc++" },
    },
  },
}
