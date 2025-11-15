vim9script

augroup mcge_lang_rust
  autocmd!
  autocmd FileType rust setlocal tabstop=4 shiftwidth=4 softtabstop=4

  autocmd FileType rust nnoremap <silent><buffer> <leader>rr :!cargo run<CR>
  autocmd FileType rust nnoremap <silent><buffer> <leader>rt :!cargo test<CR>

  autocmd FileType rust setlocal makeprg=cargo\ build
augroup END
