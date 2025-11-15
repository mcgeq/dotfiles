vim9script

import autoload "mcge_utils.vim"

# -------------------- 保存退出 start --------------------
# 保存文件
nnoremap <silent> <C-x><C-s> :w<CR>
inoremap <silent> <C-x><C-s> <Esc>:w<CR>
vnoremap <silent> <C-x><C-s> <Esc>:w<CR>

# 退出
nnoremap <silent> <C-x><C-q> :wq<CR>
inoremap <silent> <C-x><C-q> <Esc>:wq<CR>
vnoremap <silent> <C-x><C-q> <Esc>:wq<CR>

# -------------------- 保存退出 end   --------------------

# -------------------- 窗口 start --------------------
noremap <c-h> <C-w>h
noremap <c-j> <C-w>j
noremap <c-k> <C-w>k
noremap <c-l> <C-w>l
tnoremap <c-h> <c-\><c-n><c-w>h
tnoremap <c-j> <c-\><c-n><c-w>j
tnoremap <c-k> <c-\><c-n><c-w>k
tnoremap <c-l> <c-\><c-n><c-w>l


# 关闭窗口
nnoremap <silent> q <esc>:close<cr>
vnoremap <silent> q <esc>:close<cr>

nnoremap <silent> <C-q> :bdelete<cr>
inoremap <silent> <C-q> <esc>:bdelete<cr>
vnoremap <silent> <C-q> <esc>:bdelete<cr>
# -------------------- 窗口 end --------------------

# -------------------- 行     start --------------------
# 行首
nnoremap <silent> <C-a> 0
inoremap <silent> <C-a> <Esc>0
vnoremap <silent> <C-a> <Esc>0

# 行尾
nnoremap <silent> <C-e> $
inoremap <silent> <C-e> <Esc>$
vnoremap <silent> <C-e> <Esc>$

# -------------------- 行       end --------------------

# -------------------- 命令行 start --------------------
# 命令行移动
cnoremap <C-h> <Home>
cnoremap <C-l> <End>
# -------------------- 命令行 end --------------------

# -------------------- buffer start --------------------
# 使用 alt q 关闭当前 buffer
nnoremap <M-q> <esc>:bdelete<cr>

# 切换buffer
nnoremap <C-p> :bprevious<CR>
inoremap <C-p> <Esc>:bprevious<CR>
vnoremap <C-p> <Esc>:bprevious<CR>

nnoremap <C-n> :bnext<CR>
inoremap <C-n> <Esc>:bnext<CR>
vnoremap <C-n> <Esc>:bnext<CR>
# -------------------- buffer end --------------------

# -------------------- 宏 start --------------------
# 去除 EX 模式
nmap Q <nop>
# 使用 Q 进行宏录制
noremap Q q
# -------------------- 宏 end --------------------
