local source = (arg and arg[0]) or debug.getinfo(1, "S").source
source = source:gsub("^@", "")
local root = vim.fn.fnamemodify(source, ":p:h:h")

vim.opt.rtp:prepend(root)

local function module_name(path)
  local relative = path:sub(#root + 2)
  relative = relative:gsub("^lua[\\/]", "")
  return relative:gsub("[\\/]", "."):gsub("%.lua$", "")
end

local function collect_modules(dir)
  local modules = {}
  for name, type_ in vim.fs.dir(dir) do
    if type_ == "file" and name:sub(-4) == ".lua" then
      local base = name:sub(1, -5)
      if base ~= "private" and not base:find "%." then
        modules[#modules + 1] = module_name(vim.fs.joinpath(dir, name))
      end
    elseif type_ == "directory" and name ~= "private" then
      for _, nested in ipairs(collect_modules(vim.fs.joinpath(dir, name))) do
        modules[#modules + 1] = nested
      end
    end
  end
  return modules
end

local lua_root = vim.fs.joinpath(root, "lua")
local modules = collect_modules(lua_root)
table.sort(modules)

local failures = 0
for _, name in ipairs(modules) do
  local ok, err = pcall(require, name)
  if not ok then
    failures = failures + 1
    io.stderr:write(("FAIL %-40s %s\n"):format(name, tostring(err):gsub("%s+", " ")))
  end
end

if failures > 0 then
  io.stderr:write(("\n%d/%d modules failed to load\n"):format(failures, #modules))
  os.exit(1)
end

io.write(("OK   all %d modules loaded\n"):format(#modules))
