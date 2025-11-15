vim9script

set expandtab			# tab 扩展为空格
set tabstop=4
set softtabstop=4		# 连续数量的空格当成一个制表符
set shiftwidth=4

# 根据文件类型设置制表符大小

autocmd FileType rust setlocal tabstop=4 shiftwidth=4
autocmd FileType python setlocal tabstop=4 shiftwidth=4
autocmd FileType javascript setlocal tabstop=2 shiftwidth=2
autocmd FileType javascriptreact setlocal tabstop=2 shiftwidth=2
autocmd FileType typescript setlocal tabstop=2 shiftwidth=2
autocmd FileType typescriptreact setlocal tabstop=2 shiftwidth=2
autocmd FileType csharp setlocal tabstop=4 shiftwidth=4
autocmd FileType html setlocal tabstop=2 shiftwidth=2
