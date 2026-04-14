vim9script

if exists('b:did_mcge_vim_ftplugin')
  finish
endif
b:did_mcge_vim_ftplugin = true

setlocal comments-=:#

nnoremap <silent><buffer> <F5> <Cmd>source %<CR>
inoremap <silent><buffer> <F5> <Esc><Cmd>source %<CR>
