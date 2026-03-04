vim9script
# ============================================================================
# UI 外观设置
# 作者：mcge <mcgeq@outlook.com>
# ============================================================================

# 配置
var config = {
  gui_hide_toolbar: true,
  gui_hide_menubar: true,
  gui_hide_scrollbar: true,
  cmdheight: 2,
}

# GUI 选项优化
def g:InitGuiOptions()
  if has('gui_running')
    if config.gui_hide_toolbar
      set guioptions-=T               # 隐藏工具栏
    endif
    if config.gui_hide_menubar
      set guioptions-=m               # 隐藏菜单栏
    endif
    if config.gui_hide_scrollbar
      set guioptions-=r               # 隐藏右侧滚动条
      set guioptions-=l               # 隐藏左侧滚动条
      set guioptions-=b               # 隐藏底部滚动条
    endif

    call g:ErrDebug('GUI options initialized')
  endif
enddef

# 初始化 UI
def g:InitUi()
  # 命令行高度
  execute 'set cmdheight=' .. config.cmdheight

  # 初始化 GUI
  g:InitGuiOptions()

  call g:ErrDebug('UI initialized')
enddef

# 立即初始化
call g:InitUi()

# vim: set ft=vim sw=2 ts=2 sts=2 et:
