" ========================================================================
" vim-which-key 配置
" 作者：mcge <mcgeq@outlook.com>
" 描述：快捷键提示弹窗配置
" ========================================================================

" ----------------------------------------------------------------------------
" 1. 触发映射（Leader 键触发）
" ----------------------------------------------------------------------------
nnoremap <silent> <leader> :<c-u>WhichKey '<Space>'<CR>
vnoremap <silent> <leader> :<c-u>WhichKeyVisual '<Space>'<CR>

" ----------------------------------------------------------------------------
" 2. 定义描述字典
" ----------------------------------------------------------------------------
let g:which_key_map = {}

" Git 操作组（在 mapping.vim 中定义实际映射，这里只注册描述）
let g:which_key_map.g = {
    \ 'name': '+git',
    \ 'g': [':G', 'fugitive'],
    \ 's': [':Git', 'status'],
    \ 'b': [':Git blame', 'blame'],
    \ 'd': [':Gdiffsplit', 'diff'],
    \ 'w': [':Gwrite', 'write'],
    \ 'R': [':Gread', 'read'],
    \ 'm': [':Git commit', 'commit'],
    \ 'c': [':Gclog', 'commits-log'],
    \ 'o': [':copen', 'open-quickfix'],
    \ 'q': [':cclose', 'close-quickfix'],
    \ 'n': [':cnext', 'next-quickfix'],
    \ 'p': [':cprevious', 'prev-quickfix'],
    \ }

" 文件操作组
let g:which_key_map.f = {
    \ 'name': '+file',
    \ 's': [':update', 'save'],
    \ 'd': [':e $MYVIMRC', 'open-vimrc'],
    \ }

" Buffer 操作组
let g:which_key_map.b = {
    \ 'name': '+buffer',
    \ 'n': [':bnext', 'next'],
    \ 'p': [':bprevious', 'previous'],
    \ 'd': [':bd', 'delete'],
    \ }

" 窗口操作组
let g:which_key_map.w = {
    \ 'name': '+windows',
    \ 'h': ['<C-W>h', 'left'],
    \ 'j': ['<C-W>j', 'below'],
    \ 'k': ['<C-W>k', 'up'],
    \ 'l': ['<C-W>l', 'right'],
    \ 'v': ['<C-W>v', 'split-vertical'],
    \ 's': ['<C-W>s', 'split-horizontal'],
    \ '=': ['<C-W>=', 'balance'],
    \ }

" LSP 操作组
let g:which_key_map.l = {
    \ 'name': '+lsp',
    \ 'd': ['<plug>(coc-definition)', 'definition'],
    \ 'r': ['<plug>(coc-references)', 'references'],
    \ 'i': ['<plug>(coc-implementation)', 'implementation'],
    \ 't': ['<plug>(coc-type-definition)', 'type-definition'],
    \ 'n': ['<plug>(coc-diagnostic-next)', 'next-diagnostic'],
    \ 'N': ['<plug>(coc-diagnostic-prev)', 'prev-diagnostic'],
    \ 'a': ['<plug>(coc-codeaction)', 'code-action'],
    \ 'f': ['<plug>(coc-format)', 'format'],
    \ }

" 导航操作组
let g:which_key_map.n = {
    \ 'name': '+navigation',
    \ 'f': [':Clap files', 'find-files'],
    \ 'g': [':Clap grep', 'grep'],
    \ 'b': [':Clap buffers', 'buffers'],
    \ 't': [':Vista!!', 'tags'],
    \ }

" ----------------------------------------------------------------------------
" 3. 注册描述字典
" ----------------------------------------------------------------------------
call which_key#register('<Space>', 'g:which_key_map')

" ----------------------------------------------------------------------------
" 4. 隐藏状态栏（使界面更简洁）
" ----------------------------------------------------------------------------
autocmd! FileType which_key
autocmd  FileType which_key set laststatus=0 noshowmode noruler
      \| autocmd BufLeave <buffer> set laststatus=2 showmode ruler

" ----------------------------------------------------------------------------
" 5. Which-key 配色（链接到现有语法组，自动跟随主题）
" ----------------------------------------------------------------------------
" 关键：强制覆盖 WhichKeyFloating，使其背景跟随主题 Normal
hi! link WhichKeyFloating Normal
hi! link WhichKey Function
hi! link WhichKeySeperator DiffAdded
hi! link WhichKeyGroup Keyword
hi! link WhichKeyDesc Identifier
