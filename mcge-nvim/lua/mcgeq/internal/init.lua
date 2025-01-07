local au = vim.api.nvim_create_autocmd
local uc = vim.api.nvim_create_user_command
local group = vim.api.nvim_create_augroup('McgeqGroup', {})

au('TermOpen', { group = group, command = 'startinsert' })

au('TextYankPost', {
  group = group,
  callback = function()
    vim.highlight.on_yank({ higroup = 'Visual', timeout = 200 })
  end,
})

au('BufRead', {
  callback = function()
    vim.cmd.setlocal('formatoptions-=ro')
    -- last plase
    local pos = vim.fn.getpos('\'"')
    if pos[2] > 0 and pos[2] <= vim.fn.line('$') then
      vim.api.nvim_win_set_cursor(0, { pos[2], pos[3] - 1 })
    end
  end,
})

-- markdown keymap
au('FileType', {
  group = group,
  pattern = 'markdown',
  callback = function()
    require('keymap.markdown').markdown_keymap()
  end,
})

-- Enter
au('BufEnter', {
  group = group,
  once = true,
  callback = function()
    -- keymap
    require('mcgeq.keymap')
  end,
})
