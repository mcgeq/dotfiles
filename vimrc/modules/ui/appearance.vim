vim9script
# ============================================================================
# UI 外观设置
# 作者: mcge <mcgeq@outlook.com>
# ============================================================================

# GUI 选项优化
def g:InitGuiOptions()
  if has('gui_running')
    # 隐藏工具栏和菜单栏
    set guioptions-=T               # 隐藏工具栏
    set guioptions-=m               # 隐藏菜单栏
    set guioptions-=r               # 隐藏右侧滚动条
    set guioptions-=l               # 隐藏左侧滚动条
    set guioptions-=b               # 隐藏底部滚动条
    
    call g:ErrDebug('GUI options initialized')  # 改为调试级别，不显示
  endif
enddef

# 命令行高度
set cmdheight=2

# 初始化 GUI 选项
call g:InitGuiOptions()

# vim: set ft=vim sw=2 ts=2 sts=2 et:
