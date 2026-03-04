vim9script
# ============================================================================
# Editor 模块 - 括号匹配导航 (vim-matchup)
# 作者：mcge <mcgeq@outlook.com>
# ============================================================================

# 防止重复加载
if exists('g:mcge_match_pair_loaded')
  finish
endif
g:mcge_match_pair_loaded = true

# 配置
var config = {
  enabled: 1,
  matchparen_offscreen: {'method': 'popup'},
  matchparen_highlight: 1,
  delim_noskips: 2,
  mappings_enabled: 1,
  text_obj_enabled: 1,
}

# 初始化 Matchup
def g:InitMatchup(user_config: dict<any> = {})
  # 合并用户配置
  extend(config, user_config)

  if !config.enabled
    call g:ErrDebug('Matchup is disabled')
    return
  endif

  # 基础设置
  g:matchup_enabled = config.enabled
  g:matchup_matchparen_offscreen = config.matchparen_offscreen
  g:matchup_matchparen_highlight = config.matchparen_highlight
  g:matchup_delim_noskips = config.delim_noskips
  g:matchup_mappings_enabled = config.mappings_enabled
  g:matchup_text_obj_enabled = config.text_obj_enabled

  # 设置映射
  SetupMappings()

  call g:ErrDebug('Matchup initialized')
enddef

# 设置映射
def SetupMappings()
  omap i <Plug>(matchup-i%)
  omap a <Plug>(matchup-a%)
  xmap i <Plug>(matchup-i%)
  xmap a <Plug>(matchup-a%)
  nmap g% <Plug>(matchup-g%)
  nmap [% <Plug>(matchup-[%)
  nmap ]% <Plug>(matchup-]%)
  nmap z% <Plug>(matchup-z%)
enddef

# 健康检查
def g:MatchupHealthCheck(): dict<any>
  return {
    name: 'Matchup',
    available: exists(':MatchupInfo'),
    enabled: config.enabled,
    popup_supported: has('popup'),
    status: config.enabled ? 'running' : 'disabled',
  }
enddef

# 获取配置
def g:GetMatchupConfig(): dict<any>
  return config
enddef

# 立即初始化
call g:InitMatchup()

# vim: set ft=vim sw=2 ts=2 sts=2 et:
