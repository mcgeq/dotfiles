vim9script

augroup mcge_lang_python
  autocmd!
  autocmd FileType python setlocal tabstop=4 shiftwidth=4 softtabstop=4

  autocmd FileType python nnoremap <silent><buffer> <leader>rr :!python %<CR>
augroup END
