vim9script
# ============================================================================
# Editor 模块 - 多光标编辑 (vim-visual-multi)
# 作者：mcge <mcgeq@outlook.com>
# ============================================================================

# 防止重复加载
if exists('g:mcge_multi_cursor_loaded')
  finish
endif
g:mcge_multi_cursor_loaded = true

# 配置
var config = {
  enabled: true,
  highlight_matches: 'underline',
  show_warnings: 1,
  set_statusline: 1,
  silent_exit: 1,
  skip_shorter_lines: 1,
  live_editing: 1,
  reselect_first: 0,
  mouse_mappings: 1,
}

# 初始化 Visual Multi
def g:InitVisualMulti(user_config: dict<any> = {})
  # 合并用户配置
  extend(config, user_config)

  if !config.enabled
    call g:ErrDebug('VisualMulti is disabled')
    return
  endif

  # 设置配置
  g:VM_highlight_matches = config.highlight_matches
  g:VM_show_warnings = config.show_warnings
  g:VM_set_statusline = config.set_statusline
  g:VM_silent_exit = config.silent_exit
  g:VM_skip_shorter_lines = config.skip_shorter_lines
  g:VM_live_editing = config.live_editing
  g:VM_reselect_first = config.reselect_first
  g:VM_mouse_mappings = config.mouse_mappings

  call g:ErrDebug('VisualMulti initialized')
enddef

# 健康检查
def g:VisualMultiHealthCheck(): dict<any>
  return {
    name: 'VisualMulti',
    available: exists(':VMSelectCursors'),
    enabled: config.enabled,
    mouse_mappings: config.mouse_mappings,
    status: config.enabled ? 'running' : 'disabled',
  }
enddef

# 获取配置
def g:GetVisualMultiConfig(): dict<any>
  return config
enddef

# 立即初始化
call g:InitVisualMulti()

# vim: set ft=vim sw=2 ts=2 sts=2 et:
