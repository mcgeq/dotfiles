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
