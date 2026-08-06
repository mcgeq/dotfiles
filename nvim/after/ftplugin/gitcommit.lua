local augroup = vim.api.nvim_create_augroup
local autocmd = vim.api.nvim_create_autocmd

vim.opt_local.spell = true
vim.opt_local.textwidth = 72
vim.opt_local.colorcolumn = "73"
vim.opt_local.formatoptions = vim.opt_local.formatoptions + "t"

local COMMIT_TYPES = {
  { value = "feat", desc = "New feature" },
  { value = "fix", desc = "Bug fix" },
  { value = "docs", desc = "Documentation only" },
  { value = "style", desc = "Formatting, no code change" },
  { value = "refactor", desc = "Refactor, no behavior change" },
  { value = "perf", desc = "Performance improvement" },
  { value = "test", desc = "Adding or updating tests" },
  { value = "build", desc = "Build system or dependencies" },
  { value = "ci", desc = "CI configuration" },
  { value = "chore", desc = "Maintenance" },
  { value = "revert", desc = "Revert a commit" },
}

local function current_subject()
  local lines = vim.api.nvim_buf_get_lines(0, 0, 100, false)
  for index, line in ipairs(lines) do
    if line:match "%S" then return index, line end
  end
  return 1, ""
end

local function strip_existing_prefix(subject)
  return subject:gsub("^[%a_]+%([^)]*%)?%s*:%s*", "", 1):gsub("^[%a_]+%s*:%s*", "", 1)
end

local function insert_type()
  local entries = {}
  for _, entry in ipairs(COMMIT_TYPES) do
    table.insert(entries, ("%s - %s"):format(entry.value, entry.desc))
  end

  vim.ui.select(entries, { prompt = "Commit type:" }, function(choice)
    if not choice then return end
    local value = choice:match "^([%a_]+)%s%-"
    if not value then return end

    local lineno, subject = current_subject()
    local new_subject = value .. ": " .. strip_existing_prefix(subject)
    new_subject = new_subject:gsub("^(%a)", string.upper)

    vim.api.nvim_buf_set_lines(0, lineno - 1, lineno, false, { new_subject })
    vim.api.nvim_win_set_cursor(0, { lineno, #new_subject })
  end)
end

local last_warned_length = -1

local function check_subject_length()
  local _, subject = current_subject()
  if #subject > 50 and #subject ~= last_warned_length then
    last_warned_length = #subject
    vim.notify(("Commit subject is %d chars (limit 50)"):format(#subject), vim.log.levels.WARN, {
      title = "GitCommit",
    })
  end
end

autocmd({ "TextChanged", "InsertLeave" }, {
  group = augroup("gitcommit_subject_length", { clear = true }),
  callback = check_subject_length,
})

vim.keymap.set("n", "<leader>cc", insert_type, {
  buffer = true,
  silent = true,
  desc = "Insert commit type",
})
