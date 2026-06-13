vim9script
# ============================================================================
# 模块: Editor / MatchPair
# 作者: mcge <mcgeq@outlook.com>
# 说明: 配置 vim-matchup 匹配与文本对象行为。
# ============================================================================

# 防止重复加载
if g:MarkModuleLoaded('match_pair')
  finish
endif

# 配置
var config = {
  enabled: true,
  matchparen_offscreen: {'method': 'popup'},
  matchparen_highlight: 1,
  delim_noskips: 2,
  mappings_enabled: 1,
  text_obj_enabled: 1,
}

# 初始化 Matchup
def g:InitMatchup(user_config: dict<any> = {})
  config = g:ResolveModuleConfig('match_pair', config, user_config)

  if g:ModuleIsDisabled(config, 'Matchup')
    return
  endif

  # 基础设置
  g:ApplyGlobalVars({
    matchup_enabled: config.enabled,
    matchup_matchparen_offscreen: config.matchparen_offscreen,
    matchup_matchparen_highlight: config.matchparen_highlight,
    matchup_delim_noskips: config.delim_noskips,
    matchup_mappings_enabled: config.mappings_enabled,
    matchup_text_obj_enabled: config.text_obj_enabled,
  })

  call g:ErrDebug('Matchup initialized')
enddef

# 健康检查
def g:MatchupHealthCheck(): dict<any>
  return g:BuildManagedCommandModuleHealth('match_pair', 'Matchup', config, 'MatchupInfo', {
    popup_supported: has('popup'),
  })
enddef

# 获取配置
def g:GetMatchupConfig(): dict<any>
  return config
enddef

# 立即初始化
call g:InitMatchup()

# vim: set ft=vim sw=2 ts=2 sts=2 et:
