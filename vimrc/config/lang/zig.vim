vim9script

augroup mcge_lang_zig
  autocmd!
  autocmd FileType zig setlocal tabstop=4 shiftwidth=4 softtabstop=4

  autocmd FileType zig nnoremap <silent><buffer> <leader>rr :!zig run %<CR>
  autocmd FileType zig nnoremap <silent><buffer> <leader>rb :!zig build<CR>
augroup END
