vim9script
# ============================================================================
# CoC List 快捷键 - 更快的文件和文本搜索
# ============================================================================

# 文件搜索（比 FZF 快，因为内置）
nnoremap <silent> <leader>ff :CocList files<CR>

# 文本搜索（使用 rg）
nnoremap <silent> <leader>fw :CocList grep<CR>

# 当前文件符号搜索
nnoremap <silent> <leader>fo :CocList outline<CR>

# 缓冲区列表
nnoremap <silent> <leader>fb :CocList buffers<CR>

# 最近文件
nnoremap <silent> <leader>fr :CocList mru<CR>

# 命令历史
nnoremap <silent> <leader>fc :CocList cmdhistory<CR>

# Git 文件
nnoremap <silent> <leader>fg :CocList gfiles<CR>

# 恢复上次的列表
nnoremap <silent> <leader>fl :CocListResume<CR>

# vim: set ft=vim sw=2 ts=2 sts=2 et:
