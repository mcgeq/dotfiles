vim9script
# ============================================================================
# Git Gutter 模块 - Git 状态显示
# 作者：mcge <mcgeq@outlook.com>
# ============================================================================

# 防止重复加载
if exists('g:mcge_gutter_loaded')
  finish
endif
g:mcge_gutter_loaded = true

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
  # 合并用户配置
  extend(config, user_config)

  if !config.enabled
    call g:ErrDebug('Git Gutter is disabled')
    return
  endif

  # 基础设置
  g:gitgutter_enabled = 1
  g:gitgutter_async = config.async
  g:gitgutter_summary = config.summary
  g:gitgutter_override_highlight = config.override_highlight
  g:gitgutter_sign_column_always = config.sign_column_always
  g:gitgutter_preview_win_floating = config.preview_floating

  # 设置符号
  g:gitgutter_sign_added = config.signs.added
  g:gitgutter_sign_modified = config.signs.modified
  g:gitgutter_sign_removed = config.signs.removed
  g:gitgutter_sign_removed_first_line = config.signs.removed_first
  g:gitgutter_sign_modified_removed = config.signs.modified_removed

  # 设置快捷键
  SetupMappings()

  call g:ErrDebug('Git Gutter initialized')
enddef

# 设置快捷键
def SetupMappings()
  # 跳转 hunk
  nmap ]c <Plug>(GitGutterNextHunk)
  nmap [c <Plug>(GitGutterPrevHunk)

  # 操作 hunk
  nmap <leader>hs <Plug>(GitGutterStageHunk)
  nmap <leader>hu <Plug>(GitGutterUndoHunk)
  nmap <leader>hp <Plug>(GitGutterPreviewHunk)
enddef

# 健康检查
def g:GitGutterHealthCheck(): dict<any>
  return {
    name: 'Git Gutter',
    available: exists('g:gitgutter_enabled'),
    enabled: config.enabled,
    signs_configured: true,
    status: config.enabled ? 'running' : 'disabled',
  }
enddef

# 获取配置
def g:GetGitGutterConfig(): dict<any>
  return config
enddef

# 立即初始化
call g:InitGitGutter()

# vim: set ft=vim sw=2 ts=2 sts=2 et:
