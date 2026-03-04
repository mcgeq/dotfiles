vim9script
# ============================================================================
# Editor 模块 - 空白字符高亮 (vim-better-whitespace)
# 作者：mcge <mcgeq@outlook.com>
# ============================================================================

# 配置
var config = {
  enabled: 1,
  strip_on_save: 1,
  filetypes_blacklist: ['diff', 'gitcommit', 'markdown', 'xml'],
  color: 'CursorColumn',
  current_line_disabled: 1,
}

# 初始化 Better Whitespace
def g:InitBetterWhitespace(user_config: dict<any> = {})
  # 合并用户配置
  extend(config, user_config)

  if !config.enabled
    call g:ErrDebug('BetterWhitespace is disabled')
    return
  endif

  # 基础设置
  g:better_whitespace_enabled = config.enabled
  g:strip_whitespace_on_save = config.strip_on_save
  g:better_whitespace_filetypes_blacklist = config.filetypes_blacklist
  g:better_whitespace_color = config.color
  g:current_line_whitespace_disabled_actively = config.current_line_disabled

  # 设置映射
  SetupMappings()

  call g:ErrDebug('BetterWhitespace initialized')
enddef

# 设置映射
def SetupMappings()
  nmap <leader>tw :ToggleWhitespace<CR>
  nmap <leader>ts :StripWhitespace<CR>
enddef

# 健康检查
def g:BetterWhitespaceHealthCheck(): dict<any>
  return {
    name: 'BetterWhitespace',
    available: exists(':StripWhitespace'),
    enabled: config.enabled,
    strip_on_save: config.strip_on_save,
    status: config.enabled ? 'running' : 'disabled',
  }
enddef

# 获取配置
def g:GetBetterWhitespaceConfig(): dict<any>
  return config
enddef

# 立即初始化
call g:InitBetterWhitespace()

# vim: set ft=vim sw=2 ts=2 sts=2 et:
