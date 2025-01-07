--- Utils

local fn = vim.fn
local version = vim.version

local M = {}

--- executable
--- @param name
--- @return boolean
function M.executable(name)
   if fn.executable(name) > 0 then
      return true
   end
   return false
end

--- check whether a feature exists in Nvim
--- @param feat string the feature name, like `nvim-0.10` or `mac`.
--- @return boolean
M.has = function(feat)
   if fn.has(feat) == 1 then
      return true
   end
   return false
end

--- Create a directory if it does not exist
--- @param dir string directory name
--- @return boolean
function M.mg_create_dir(dir)
   local res = fn.isdirectory(dir)
   if res == 0 then
      fn.mkdir(dir, "p")
   end
end

--- Check if the current nvim version is compatible with the allowed version.
--- @param expected_version string
--- @return boolean
function M.mg_is_compatible_version(expected_version)
   -- check if we have the latest stable version of nvim
   local ver_expect = version.parse(expected_version)
   local ver_actual = vim.version()

   if ver_expect == nil then
      local msg = string.format("Unsupported version string: %s", expected_version)
      vim.api.nvim_err_writeln(msg)
      return false
   end

   local result = version.cmp(ver_expect, ver_actual)
   if result ~= 0 then
      local _ver = string.format("%s.%s.%s", ver_actual.major, ver_actual.minor, ver_actual.patch)
      local msg = string.format(
         "Expect nvim version %s, but your current nvim version is %s. Use at your own risk!",
         expected_version,
         _ver)
      vim.api.nvim_err_writeln(msg)
   end
   return true
end

return M
