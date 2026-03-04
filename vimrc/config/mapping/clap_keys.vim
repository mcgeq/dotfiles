vim9script
# ============================================================================
# Clap 快捷键配置
# vim-clap: 现代化的模糊搜索插件
# ============================================================================

# ----------------------------------------------------------------------------
# 文件和缓冲区搜索
# ----------------------------------------------------------------------------

# 文件搜索
nnoremap <silent> <leader>p :Clap files<CR>
nnoremap <silent> <leader>P :Clap gfiles<CR>
nnoremap <silent> <leader><Space> :Clap files<CR>

# 缓冲区
nnoremap <silent> <leader>bb :Clap buffers<CR>

# 最近文件
nnoremap <silent> <leader>fh :Clap history<CR>

# ----------------------------------------------------------------------------
# 内容搜索
# ----------------------------------------------------------------------------

# 文本搜索（使用 ripgrep）
nnoremap <silent> <leader>/ :Clap grep<CR>
nnoremap <silent> <leader>fg :Clap grep<CR>

# 智能 grep 搜索（组合 filer + grep）
nnoremap <silent> <leader>st :Clap igrep<CR>

# 普通 grep 搜索
nnoremap <silent> <leader>sg :Clap grep<CR>
xnoremap <silent> <leader>sg :Clap grep --query=@visual<CR>

# 搜索光标下单词/选中文本
nnoremap <silent> <leader>sw :Clap grep --query=<cword><CR>
xnoremap <silent> <leader>sw :Clap grep --query=@visual<CR>

# 当前文件行搜索
nnoremap <silent> <leader>fl :Clap blines<CR>

# 所有打开文件行搜索
nnoremap <silent> <leader>fL :Clap lines<CR>

# ----------------------------------------------------------------------------
# Git 相关
# ----------------------------------------------------------------------------

# Git 提交历史
nnoremap <silent> <leader>gc :Clap commits<CR>

# Git 差异文件
nnoremap <silent> <leader>gd :Clap git_diff_files<CR>

# Git 文件（当前仓库）
nnoremap <silent> <leader>gf :Clap gfiles<CR>

# ----------------------------------------------------------------------------
# Vim 功能
# ----------------------------------------------------------------------------

# 命令搜索
nnoremap <silent> <leader>: :Clap command<CR>

# 命令历史
nnoremap <silent> <leader>; :Clap command_history<CR>

# 键位映射
nnoremap <silent> <leader>km :Clap maps<CR>

# 帮助标签
nnoremap <silent> <leader>? :Clap help_tags<CR>

# 配色方案
nnoremap <silent> <leader>tc :Clap colors<CR>

# ----------------------------------------------------------------------------
# 导航
# ----------------------------------------------------------------------------

# 标记
nnoremap <silent> <leader>fm :Clap marks<CR>

# 跳转列表
nnoremap <silent> <leader>fj :Clap jumps<CR>

# 寄存器
nnoremap <silent> <leader>fr :Clap registers<CR>

# vim: set ft=vim sw=2 ts=2 sts=2 et:
