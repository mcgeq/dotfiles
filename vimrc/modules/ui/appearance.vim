vim9script
# ============================================================================
# 模块: UI / Appearance
# 作者: mcge <mcgeq@outlook.com>
# 说明: 配置 GUI 外观选项和通用界面参数。
# ============================================================================

# 防止重复加载
if g:MarkModuleLoaded('appearance')
  finish
endif

# 配置
var config = {
  enabled: true,
  gui_hide_toolbar: true,
  gui_hide_menubar: true,
  gui_hide_scrollbar: true,
  terminal_truecolor: true,
  cmdheight: 2,
}

# GUI 选项优化
def ApplyGuiOptions()
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

def ApplyTerminalUiOptions()
  if has('gui_running')
    return
  endif

  if config.terminal_truecolor && has('termguicolors')
    set termguicolors
  elseif exists('+t_Co')
    # 退回 256 色，避免终端主题完全失真。
    &t_Co = '256'
  endif
enddef

# 初始化 UI
def g:InitAppearance(user_config: dict<any> = {})
  config = g:ResolveModuleConfig('appearance', config, user_config)

  if g:ModuleIsDisabled(config, 'Appearance')
    return
  endif

  # 命令行高度
  &cmdheight = config.cmdheight

  # 初始化 GUI / 终端 UI
  ApplyGuiOptions()
  ApplyTerminalUiOptions()

  call g:ErrDebug('Appearance initialized')
enddef

def g:AppearanceHealthCheck(): dict<any>
  return g:BuildManagedModuleHealth('appearance', 'Appearance', config, {
    gui_running: has('gui_running'),
    cmdheight: config.cmdheight,
    terminal_truecolor: config.terminal_truecolor,
    termguicolors_enabled: exists('+termguicolors') ? &termguicolors : false,
    toolbar_hidden: config.gui_hide_toolbar,
    menubar_hidden: config.gui_hide_menubar,
    scrollbar_hidden: config.gui_hide_scrollbar,
  })
enddef

def g:GetAppearanceConfig(): dict<any>
  return config
enddef

# 兼容旧入口，后续统一以 g:InitAppearance() 为准。
def g:InitUi()
  g:InitAppearance()
enddef

# 立即初始化
call g:InitAppearance()

# vim: set ft=vim sw=2 ts=2 sts=2 et:
