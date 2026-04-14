vim9script

if exists('b:did_mcge_python_ftplugin')
  finish
endif
b:did_mcge_python_ftplugin = true

nnoremap <silent><buffer> <leader>rr <Cmd>!python %<CR>
