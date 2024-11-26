vim9script

autocmd FileType vim setlocal comments-=:#

nnoremap <silent><buffer> <F5> :source %<CR>
inoremap <silent><buffer> <F5> <Esc>:source %<CR>

