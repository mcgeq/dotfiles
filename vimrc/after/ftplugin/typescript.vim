vim9script

if exists('b:did_mcge_typescript_ftplugin')
  finish
endif
b:did_mcge_typescript_ftplugin = true

nnoremap <silent><buffer> <leader>rr <Cmd>!pnpm test<CR>
