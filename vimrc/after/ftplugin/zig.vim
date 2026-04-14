vim9script

if exists('b:did_mcge_zig_ftplugin')
  finish
endif
b:did_mcge_zig_ftplugin = true

nnoremap <silent><buffer> <leader>rr <Cmd>!zig run %<CR>
nnoremap <silent><buffer> <leader>rb <Cmd>!zig build<CR>
