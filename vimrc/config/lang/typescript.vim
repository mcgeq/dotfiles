vim9script

augroup mcge_lang_typescript
  autocmd!
  autocmd FileType typescript,typescriptreact setlocal tabstop=2 shiftwidth=2 softtabstop=2

  autocmd FileType typescript,typescriptreact nnoremap <silent><buffer> <leader>rr :!pnpm test<CR>
augroup END
