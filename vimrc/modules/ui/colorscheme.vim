vim9script
# ============================================================================
# 配色方案配置
# 作者: mcge <mcgeq@outlook.com>
# ============================================================================

# 默认配置
var config = {
  scheme: 'molokai',
  background: 'dark',
  enable_italic: false,
  transparent: false,
}

# 初始化配色方案
def g:InitColorscheme(user_config: dict<any> = {})
  # 合并用户配置
  extend(config, user_config)
  
  # 设置背景
  execute 'set background=' .. config.background
  
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
    execute 'colorscheme ' .. config.scheme
  catch
    call g:ErrWarn($'配色方案 {config.scheme} 未找到，使用默认方案')
    try
      colorscheme desert
    catch
      call g:ErrError('无法加载任何配色方案')
    endtry
  endtry
  
  # 透明背景设置
  if config.transparent
    hi Normal guibg=NONE ctermbg=NONE
    hi NonText guibg=NONE ctermbg=NONE
    hi LineNr guibg=NONE ctermbg=NONE
    hi SignColumn guibg=NONE ctermbg=NONE
  endif
  
  # Clap UI 自定义高亮（更美观的配色）
  call g:SetupClapHighlights()
enddef

# Clap 自定义高亮配置
def g:SetupClapHighlights()
  # 输入框样式
  hi ClapInput guifg=#C0CAF5 guibg=#1f2335 gui=bold
  hi ClapSpinner guifg=#7dcfff gui=bold
  hi ClapSearchText guifg=#ff9e64 gui=bold
  
  # 显示区域
  hi ClapDisplay guibg=#1a1b26
  hi ClapPreview guibg=#24283b
  
  # 匹配高亮
  hi ClapMatches guifg=#ff9e64 gui=bold,underline
  hi ClapNoMatchesFound guifg=#f7768e gui=bold
  
  # 选中项
  hi ClapSelected guifg=#7dcfff guibg=#283457 gui=bold
  hi ClapCurrentSelection guifg=#7dcfff guibg=#364A82 gui=bold
  
  # 分隔符和边框
  hi ClapSeparator guifg=#565f89
  hi ClapBorder guifg=#565f89
  
  # Provider 相关
  hi ClapProviderColon guifg=#9d7cd8
  hi ClapProviderAbout guifg=#9ece6a
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

# 命令定义
command! ToggleBackground call g:ToggleBackground()
command! ToggleTransparency call g:ToggleTransparency()

# 初始化
call g:InitColorscheme()

# vim: set ft=vim sw=2 ts=2 sts=2 et:
