vim9script
# ============================================================================
# 模块: UI / Airline
# 作者: mcge <mcgeq@outlook.com>
# 说明: 配置 Airline 状态栏与 tabline 扩展。
# ============================================================================

# 防止重复加载
if g:MarkModuleLoaded('airline')
  finish
endif

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
  config = g:ResolveModuleConfig('airline', config, user_config)

  if g:ModuleIsDisabled(config, 'Airline')
    return
  endif

  # 主题、扩展和分隔符
  g:ApplyGlobalVars({
    airline_theme: config.theme,
    airline_powerline_fonts: config.powerline_fonts,
    'airline#extensions#tabline#enabled': config.tabline_enabled,
    'airline#extensions#tabline#left_sep': config.tabline_left_sep,
    'airline#extensions#tabline#left_alt_sep': config.tabline_left_alt_sep,
    'airline#extensions#tabline#formatter': config.tabline_formatter,
    'airline#extensions#tabline#buffer_nr_show': config.tabline_buffer_nr_show,
    'airline#extensions#battery#enabled': config.battery_enabled,
    'airline#extensions#whitespace#mixed_indent_always': config.whitespace_mixed_indent_always,
    airline_left_sep: config.left_sep,
    airline_left_alt_sep: config.left_alt_sep,
  })

  call g:ErrDebug('Airline initialized')
enddef

# 健康检查
def g:AirlineHealthCheck(): dict<any>
  return g:BuildManagedCommandModuleHealth('airline', 'Airline', config, 'AirlineTheme', {
    theme: config.theme,
    tabline_enabled: config.tabline_enabled,
    powerline_fonts: config.powerline_fonts,
  })
enddef

# 获取配置
def g:GetAirlineConfig(): dict<any>
  return config
enddef

# 立即初始化
call g:InitAirline()

# vim: set ft=vim sw=2 ts=2 sts=2 et:
