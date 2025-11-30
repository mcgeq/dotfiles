vim9script
# ============================================================================
# 状态栏配置 (vim-airline)
# 作者: mcge <mcgeq@outlook.com>
# ============================================================================

# 检查 airline 是否可用
def IsAirlineAvailable(): bool
  return exists('g:loaded_airline')
enddef

# 默认配置
var config = {
  theme: 'molokai',
  powerline_fonts: true,
  enable_tabline: true,
  enable_battery: true,
  show_buffer_number: true,
}

# 初始化状态栏
def g:InitStatusline(user_config: dict<any> = {})
  # 合并用户配置
  extend(config, user_config)
  
  # Tabline 配置
  if config.enable_tabline
    g:airline#extensions#tabline#enabled = 1
    g:airline#extensions#tabline#left_sep = ' '
    g:airline#extensions#tabline#left_alt_sep = '|'
    g:airline#extensions#tabline#formatter = 'unique_tail'
    
    if config.show_buffer_number
      g:airline#extensions#tabline#buffer_nr_show = 1
      g:airline#extensions#tabline#buffer_nr_format = '%s:'
    endif
  endif
  
  # 扩展配置
  if config.enable_battery
    g:airline#extensions#battery#enabled = 1
  endif
  
  g:airline#extensions#whitespace#mixed_indent_always = 0
  
  # Powerline 字体
  if config.powerline_fonts
    g:airline_powerline_fonts = 1
  endif
  
  # 分隔符
  g:airline_left_sep = ' '
  g:airline_left_alt_sep = '|'
  
  # 主题设置
  g:airline_theme = config.theme
  
  # Buffer 切换快捷键
  nnoremap <C-tab> :bn<CR>
  nnoremap <C-s-tab> :bp<CR>
  
  call g:ErrDebug('Statusline initialized with airline')  # 改为调试级别
enddef

# 切换主题
def g:SetAirlineTheme(theme: string)
  config.theme = theme
  g:airline_theme = theme
  AirlineRefresh
enddef

# 获取配置
def g:GetStatuslineConfig(): dict<any>
  return config
enddef

# 延迟初始化（等待 airline 加载）
timer_start(100, (_) => {
  if IsAirlineAvailable()
    call g:InitStatusline()
  else
    call g:ErrWarn('vim-airline not found, using default statusline')
  endif
})

# vim: set ft=vim sw=2 ts=2 sts=2 et:
