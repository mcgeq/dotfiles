vim9script
# ============================================================================
# Git 模块 - 快捷键映射
# 作者：mcge <mcgeq@outlook.com>
# ============================================================================

# 设置 Git 相关快捷键
def SetupGitMappings()
  # Fugitive Git 命令
  nnoremap <silent><nowait> <leader>gg  :G<CR>
  nnoremap <silent><nowait> <leader>gb  :<c-u>Git blame<CR>
  nnoremap <silent><nowait> <leader>gd  :<c-u>Gdiffsplit<CR>
  nnoremap <silent><nowait> <leader>gw  :<c-u>Gwrite<CR>
  nnoremap <silent><nowait> <leader>gr  :<c-u>Gread<CR>
  nnoremap <silent><nowait> <leader>gm  :<c-u>Git commit<CR>
  nnoremap <silent><nowait> <leader>gc  :<c-u>Gclog<CR>
  nnoremap <silent><nowait> <leader>go  :<c-u>copen<CR>
  nnoremap <silent><nowait> <leader>gq  :<c-u>cclose<CR>
  nnoremap <silent><nowait> <leader>gn  :<c-u>cnext<CR>
  nnoremap <silent><nowait> <leader>gp  :<c-u>cprevious<CR>

  # Insert 模式映射
  inoremap <silent><nowait> <leader>gg  <esc>:<c-u>G<CR>
  inoremap <silent><nowait> <leader>gb  <Esc>:<c-u>Git blame<CR>
  inoremap <silent><nowait> <leader>gd  <Esc>:<c-u>Gdiffsplit<CR>
  inoremap <silent><nowait> <leader>gw  <Esc>:<c-u>Gwrite<CR>
  inoremap <silent><nowait> <leader>gr  <Esc>:<c-u>Gread<CR>
  inoremap <silent><nowait> <leader>gm  <Esc>:<c-u>Git commit<CR>
  inoremap <silent><nowait> <leader>gc  <Esc>:<c-u>Gclog<CR>
enddef

# 立即设置映射
SetupGitMappings()

# vim: set ft=vim sw=2 ts=2 sts=2 et:
