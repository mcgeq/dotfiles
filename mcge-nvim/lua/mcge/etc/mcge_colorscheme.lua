local M = {}

M.reset = function()
  local colorscheme = require("mcge.core").config.colorscheme
  ---@diagnostic disable-next-line: param-type-mismatch
  local status_ok, _ = pcall(vim.cmd, "colorscheme " .. colorscheme)
  if not status_ok then
    vim.notify("colorscheme: " .. colorscheme .. " not found！")
    return
  end
end

return M
