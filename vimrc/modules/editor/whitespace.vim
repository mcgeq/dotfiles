vim9script
# ============================================================================
# 模块: Editor / Whitespace
# 作者: mcge <mcgeq@outlook.com>
# 说明: 配置空白字符高亮和保存清理行为。
# ============================================================================

# 防止重复加载
if g:MarkModuleLoaded('whitespace')
  finish
endif

# 配置
var config = {
  enabled: true,
  strip_on_save: 1,
  filetypes_blacklist: ['diff', 'gitcommit', 'markdown', 'xml', 'startify'],
  color: 'CursorColumn',
  current_line_disabled: 1,
}

# 初始化 Better Whitespace
def g:InitBetterWhitespace(user_config: dict<any> = {})
  config = g:ResolveModuleConfig('whitespace', config, user_config)

  if g:ModuleIsDisabled(config, 'BetterWhitespace')
    return
  endif

  # 基础设置
  g:ApplyGlobalVars({
    better_whitespace_enabled: config.enabled,
    strip_whitespace_on_save: config.strip_on_save,
    better_whitespace_filetypes_blacklist: config.filetypes_blacklist,
    better_whitespace_color: config.color,
    current_line_whitespace_disabled_actively: config.current_line_disabled,
  })

  call g:ErrDebug('BetterWhitespace initialized')
enddef

# 健康检查
def g:BetterWhitespaceHealthCheck(): dict<any>
  return g:BuildManagedCommandModuleHealth('whitespace', 'BetterWhitespace', config, 'StripWhitespace', {
    strip_on_save: config.strip_on_save,
  })
enddef

# 获取配置
def g:GetBetterWhitespaceConfig(): dict<any>
  return config
enddef

# 立即初始化
call g:InitBetterWhitespace()

# vim: set ft=vim sw=2 ts=2 sts=2 et:
