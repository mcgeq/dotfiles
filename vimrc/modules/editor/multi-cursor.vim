vim9script
# ============================================================================
# 模块: Editor / MultiCursor
# 作者: mcge <mcgeq@outlook.com>
# 说明: 配置 vim-visual-multi 多光标编辑行为。
# ============================================================================

# 防止重复加载
if g:MarkModuleLoaded('multi_cursor')
  finish
endif

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
  config = g:ResolveModuleConfig('multi_cursor', config, user_config)

  if g:ModuleIsDisabled(config, 'VisualMulti')
    return
  endif

  # 设置配置
  g:ApplyGlobalVars({
    VM_highlight_matches: config.highlight_matches,
    VM_show_warnings: config.show_warnings,
    VM_set_statusline: config.set_statusline,
    VM_silent_exit: config.silent_exit,
    VM_skip_shorter_lines: config.skip_shorter_lines,
    VM_live_editing: config.live_editing,
    VM_reselect_first: config.reselect_first,
    VM_mouse_mappings: config.mouse_mappings,
  })

  call g:ErrDebug('VisualMulti initialized')
enddef

# 健康检查
def g:VisualMultiHealthCheck(): dict<any>
  return g:BuildManagedCommandModuleHealth('multi_cursor', 'VisualMulti', config, 'VMSelectCursors', {
    mouse_mappings: config.mouse_mappings,
  })
enddef

# 获取配置
def g:GetVisualMultiConfig(): dict<any>
  return config
enddef

# 立即初始化
call g:InitVisualMulti()

# vim: set ft=vim sw=2 ts=2 sts=2 et:
