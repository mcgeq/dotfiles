vim9script
# ============================================================================
# 模块: Git / Gutter
# 作者: mcge <mcgeq@outlook.com>
# 说明: 配置 GitGutter 符号和预览行为。
# ============================================================================

# 防止重复加载
if g:MarkModuleLoaded('gutter')
  finish
endif

# 配置
var config = {
  enabled: true,
  signs: {
    added: '│',
    modified: '┌',
    removed: '╵',
    removed_first: '█',
    modified_removed: '╜',
  },
  async: true,
  summary: true,
  override_highlight: false,
  sign_column_always: true,
  preview_floating: true,
}

# 初始化 Git Gutter
def g:InitGitGutter(user_config: dict<any> = {})
  config = g:ResolveModuleConfigDeep('gutter', config, user_config)

  if g:ModuleIsDisabled(config, 'Git Gutter')
    return
  endif

  # 基础设置和符号
  g:ApplyGlobalVars({
    gitgutter_enabled: 1,
    gitgutter_async: config.async,
    gitgutter_summary: config.summary,
    gitgutter_override_highlight: config.override_highlight,
    gitgutter_sign_column_always: config.sign_column_always,
    gitgutter_preview_win_floating: config.preview_floating,
    gitgutter_sign_added: config.signs.added,
    gitgutter_sign_modified: config.signs.modified,
    gitgutter_sign_removed: config.signs.removed,
    gitgutter_sign_removed_first_line: config.signs.removed_first,
    gitgutter_sign_modified_removed: config.signs.modified_removed,
  })

  call g:ErrDebug('Git Gutter initialized')
enddef

# 健康检查
def g:GitGutterHealthCheck(): dict<any>
  return g:BuildManagedModuleHealth('gutter', 'Git Gutter', config, {
    available: exists('g:gitgutter_enabled'),
    signs_configured: true,
  })
enddef

# 获取配置
def g:GetGitGutterConfig(): dict<any>
  return config
enddef

# 立即初始化
call g:InitGitGutter()

# vim: set ft=vim sw=2 ts=2 sts=2 et:
