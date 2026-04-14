vim9script
# ============================================================================
# 组件: After / Ftplugin
# 作者: mcge <mcgeq@outlook.com>
# 说明: 新文件类型局部配置模板，复制后按实际 filetype 重命名。
# ============================================================================

if exists('b:did_mcge_ftplugin')
  finish
endif
b:did_mcge_ftplugin = true

# 这里适合放 buffer-local 行为：
# - setlocal / &l: 选项
# - <buffer> 映射
# - makeprg / compiler / formatoptions

# 示例：
# setlocal commentstring=#\ %s
# &l:makeprg = 'tool build'
# nnoremap <silent><buffer> <leader>rr <Cmd>!tool run %<CR>

# vim: set ft=vim sw=2 ts=2 sts=2 et:
