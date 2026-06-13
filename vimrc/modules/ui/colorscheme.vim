vim9script
# ============================================================================
# 模块: UI / Colorscheme
# 作者: mcge <mcgeq@outlook.com>
# 说明: 配置主题、背景和相关高亮样式。
# ============================================================================

# 防止重复加载
if g:MarkModuleLoaded('colorscheme')
  finish
endif

# 配置
var config = {
  enabled: true,
  scheme: 'molokai',
  background: 'dark',
  enable_italic: false,
  transparent: false,
}

def ApplyHighlight(group: string, specs: list<string>)
  execute 'highlight ' .. group .. ' ' .. join(specs, ' ')
enddef

def g:ApplyColorschemeExtras()
  if config.transparent
    highlight Normal guibg=NONE ctermbg=NONE
    highlight NonText guibg=NONE ctermbg=NONE
    highlight LineNr guibg=NONE ctermbg=NONE
    highlight SignColumn guibg=NONE ctermbg=NONE
  endif

  ApplyClapHighlights()
enddef

# 初始化配色方案
def g:InitColorscheme(user_config: dict<any> = {})
  config = g:ResolveModuleConfig('colorscheme', config, user_config)

  if g:ModuleIsDisabled(config, 'Colorscheme')
    return
  endif

  # 设置背景
  &background = config.background

  # Molokai 特定设置
  if config.scheme == 'molokai'
    g:molokai_original = 1
  endif

  # Gruvbox 特定设置
  if config.scheme == 'gruvbox'
    g:gruvbox_italic = config.enable_italic ? 1 : 0
  endif

  # 应用配色方案
  try
    execute 'colorscheme ' .. fnameescape(config.scheme)
  catch
    call g:ErrWarn($'配色方案 {config.scheme} 未找到，使用默认方案')
    try
      colorscheme desert
    catch
      call g:ErrError('无法加载任何配色方案')
    endtry
  endtry

  g:ApplyColorschemeExtras()
enddef

# Clap 自定义高亮配置
def ApplyClapHighlights()
  # 输入框样式
  ApplyHighlight('ClapInput', ['guifg=#C0CAF5', 'ctermfg=153', 'guibg=#1f2335', 'ctermbg=235', 'gui=bold', 'cterm=bold'])
  ApplyHighlight('ClapSpinner', ['guifg=#7dcfff', 'ctermfg=117', 'gui=bold', 'cterm=bold'])
  ApplyHighlight('ClapSearchText', ['guifg=#ff9e64', 'ctermfg=215', 'gui=bold', 'cterm=bold'])

  # 显示区域
  ApplyHighlight('ClapDisplay', ['guibg=#1a1b26', 'ctermbg=234'])
  ApplyHighlight('ClapPreview', ['guibg=#24283b', 'ctermbg=236'])

  # 匹配高亮
  ApplyHighlight('ClapMatches', ['guifg=#ff9e64', 'ctermfg=215', 'gui=bold,underline', 'cterm=bold,underline'])
  ApplyHighlight('ClapNoMatchesFound', ['guifg=#f7768e', 'ctermfg=210', 'gui=bold', 'cterm=bold'])

  # 选中项
  ApplyHighlight('ClapSelected', ['guifg=#7dcfff', 'ctermfg=117', 'guibg=#283457', 'ctermbg=24', 'gui=bold', 'cterm=bold'])
  ApplyHighlight('ClapCurrentSelection', ['guifg=#7dcfff', 'ctermfg=117', 'guibg=#364A82', 'ctermbg=60', 'gui=bold', 'cterm=bold'])

  # 分隔符和边框
  ApplyHighlight('ClapSeparator', ['guifg=#565f89', 'ctermfg=60'])
  ApplyHighlight('ClapBorder', ['guifg=#565f89', 'ctermfg=60'])

  # Provider 相关
  ApplyHighlight('ClapProviderColon', ['guifg=#9d7cd8', 'ctermfg=140'])
  ApplyHighlight('ClapProviderAbout', ['guifg=#9ece6a', 'ctermfg=114'])
enddef

# 切换背景色
def g:ToggleBackground()
  if &background == 'dark'
    set background=light
  else
    set background=dark
  endif
enddef

# 切换透明度
def g:ToggleTransparency()
  config.transparent = !config.transparent
  call g:InitColorscheme()
enddef

# 获取当前配置
def g:GetColorschemeConfig(): dict<any>
  return config
enddef

def g:ColorschemeHealthCheck(): dict<any>
  return g:BuildManagedModuleHealth('colorscheme', 'Colorscheme', config, {
    scheme: config.scheme,
    background: config.background,
    transparent: config.transparent,
    active_scheme: exists('g:colors_name') ? g:colors_name : '',
  })
enddef

# 命令定义
command! ToggleBackground call g:ToggleBackground()
command! ToggleTransparency call g:ToggleTransparency()

# 立即初始化
call g:InitColorscheme()

# vim: set ft=vim sw=2 ts=2 sts=2 et:
