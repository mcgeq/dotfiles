local M = {}

function M.quickfix_prev()
  local ok = pcall(vim.cmd.cprevious)
  if not ok then vim.notify("No previous quickfix item.", vim.log.levels.INFO, { title = "Quickfix" }) end
end

function M.quickfix_next()
  local ok = pcall(vim.cmd.cnext)
  if not ok then vim.notify("No next quickfix item.", vim.log.levels.INFO, { title = "Quickfix" }) end
end

function M.loclist_prev()
  local ok = pcall(vim.cmd.lprevious)
  if not ok then vim.notify("No previous location list item.", vim.log.levels.INFO, { title = "Location List" }) end
end

function M.loclist_next()
  local ok = pcall(vim.cmd.lnext)
  if not ok then vim.notify("No next location list item.", vim.log.levels.INFO, { title = "Location List" }) end
end

function M.clear_quickfix()
  vim.fn.setqflist {}
  pcall(vim.cmd.cclose)
  vim.notify("Quickfix list cleared.", vim.log.levels.INFO, { title = "Quickfix" })
end

function M.clear_loclist()
  vim.fn.setloclist(0, {})
  pcall(vim.cmd.lclose)
  vim.notify("Location list cleared.", vim.log.levels.INFO, { title = "Location List" })
end

return M
