local M = {}

function M.new()
  if
    require("user").private.features.database ~= false
    and require("pack.loader").ensure_many { "vim-dadbod", "vim-dadbod-completion" }
  then
    local ok, source = pcall(require, "vim_dadbod_completion.blink")
    if ok then return source.new() end
  end
  return {
    enabled = function() return false end,
    get_completions = function(_, _, callback) callback { items = {}, is_incomplete_forward = false } end,
  }
end

return M
