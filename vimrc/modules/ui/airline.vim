vim9script
# ============================================================================
# UI 模块 - Airline 状态栏配置
# 作者：mcge <mcgeq@outlook.com>
# ============================================================================

# 防止重复加载
if exists('g:mcge_airline_loaded')
  finish
endif
g:mcge_airline_loaded = true

# 配置
var config = {
  enabled: true,
  theme: 'molokai',
  powerline_fonts: 1,
  tabline_enabled: true,
  tabline_left_sep: ' ',
  tabline_left_alt_sep: '|',
  tabline_formatter: 'unique_tail',
  tabline_buffer_nr_show: true,
  battery_enabled: true,
  whitespace_mixed_indent_always: false,
  left_sep: ' ',
  left_alt_sep: '|',
}

# 初始化 Airline
def g:InitAirline(user_config: dict<any> = {})
  # 合并用户配置
  extend(config, user_config)

  if !config.enabled
    call g:ErrDebug('Airline is disabled')
    return
  endif

  # 主题设置
  g:airline_theme = config.theme
  g:airline_powerline_fonts = config.powerline_fonts

  # Tabline 配置
  g:airline#extensions#tabline#enabled = config.tabline_enabled
  g:airline#extensions#tabline#left_sep = config.tabline_left_sep
  g:airline#extensions#tabline#left_alt_sep = config.tabline_left_alt_sep
  g:airline#extensions#tabline#formatter = config.tabline_formatter
  g:airline#extensions#tabline#buffer_nr_show = config.tabline_buffer_nr_show

  # 扩展配置
  g:airline#extensions#battery#enabled = config.battery_enabled
  g:airline#extensions#whitespace#mixed_indent_always = config.whitespace_mixed_indent_always

  # 分隔符
  g:airline_left_sep = config.left_sep
  g:airline_left_alt_sep = config.left_alt_sep

  # 设置快捷键
  SetupMappings()

  call g:ErrDebug('Airline initialized')
enddef

# 设置快捷键
def SetupMappings()
  # 切换 buffer
  nnoremap <C-tab> :bn<CR>
  nnoremap <C-s-tab> :bp<CR>
enddef

# 健康检查
def g:AirlineHealthCheck(): dict<any>
  return {
    name: 'Airline',
    available: exists(':AirlineTheme'),
    enabled: config.enabled,
    theme: config.theme,
    tabline_enabled: config.tabline_enabled,
    powerline_fonts: config.powerline_fonts,
    status: config.enabled ? 'running' : 'disabled',
  }
enddef

# 获取配置
def g:GetAirlineConfig(): dict<any>
  return config
enddef

# 立即初始化
call g:InitAirline()

# vim: set ft=vim sw=2 ts=2 sts=2 et:
