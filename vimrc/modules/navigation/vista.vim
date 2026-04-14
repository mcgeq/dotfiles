vim9script
# ============================================================================
# 模块: Navigation / Vista
# 作者: mcge <mcgeq@outlook.com>
# 说明: 配置 Vista 大纲视图和 ctags 集成。
# ============================================================================

# 防止重复加载
if g:MarkModuleLoaded('vista')
  finish
endif

# 配置
var config = {
  enabled: true,
  default_executive: 'coc',
  sidebar_width: 35,
  icon_indent: ["▸ ", ""],
  highlight_whole_line: 1,
  enable_icon: 1,
  cursor_delay: 200,
  floating_preview: 1,
  floating_border: 'rounded',
  focus_on_icons: 1,
  update_on_text_changed: 0,
  update_on_text_changed_delay: 500,
  echo_cursor: 1,
  close_on_jump: 0,
  stay_on_open: 0,
  sidebar_position: 'vertical topleft',
}

# 图标映射
var icons = {
  'function': "\uf794",
  'variable': "\uf71b",
  'prototype': "\uf794",
  'macro': "\uf8a3",
  'class': "\uf0e8",
  'method': "\uf6a6",
  'struct': "\ufb44",
  'interface': "\ufa52",
  'enum': "\uf779",
  'field': "\uf93d",
  'constant': "\uf8ff",
  'module': "\uf668",
  'namespace': "\uf475",
  'package': "\ue612",
  'type': "\uf7fd",
  'typedef': "\uf7fd",
  'parameter': "\uf4a1",
  'default': "\uf29c",
}

# 初始化 Vista
def g:InitVista(user_config: dict<any> = {})
  config = g:ResolveModuleConfig('vista', config, user_config)

  if g:ModuleIsDisabled(config, 'Vista')
    return
  endif

  # Executive 配置
  config.default_executive = get(g:, 'mcge_custom_vista_executive', config.default_executive)
  g:ApplyGlobalVars({
    vista_default_executive: config.default_executive,
    vista_executive_for: {
      'vim': 'ctags',
      'python': 'coc',
      'javascript': 'coc',
      'typescript': 'coc',
      'rust': 'coc',
      'go': 'coc',
      'c': 'coc',
      'cpp': 'coc',
      'java': 'coc',
    },
    vista_sidebar_width: config.sidebar_width,
    vista_icon_indent: config.icon_indent,
    vista_highlight_whole_line: config.highlight_whole_line,
    'vista#renderer#enable_icon': config.enable_icon,
    vista_cursor_delay: config.cursor_delay,
    vista_floating_preview: config.floating_preview,
    vista_floating_border: config.floating_border,
    vista_focus_on_icons: config.focus_on_icons,
    vista_update_on_text_changed: config.update_on_text_changed,
    vista_update_on_text_changed_delay: config.update_on_text_changed_delay,
    vista_echo_cursor: config.echo_cursor,
    vista_close_on_jump: config.close_on_jump,
    vista_stay_on_open: config.stay_on_open,
    vista_sidebar_position: config.sidebar_position,
    'vista#renderer#icons': icons,
  })

  # Ctags 配置
  ConfigureCtags()

  call g:ErrDebug('Vista initialized')
enddef

# 设置 Ctags
def ConfigureCtags()
  if executable('ctags')
    g:vista_ctags_cmd = {
      'haskell': 'hasktags -x -o - -c',
    }
    g:vista_ctags_options = '--fields=+niazS --extras=+q'
    g:vista_ctags_project_opts = '--recurse'
  endif
enddef

# 健康检查
def g:VistaHealthCheck(): dict<any>
  var ctags_available = executable('ctags')
  var coc_available = exists('*CocAction')

  return g:BuildManagedCommandModuleHealth('vista', 'Vista', config, 'Vista', {
    default_executive: config.default_executive,
    ctags_available: ctags_available,
    coc_available: coc_available,
    icons_configured: true,
  })
enddef

# 获取配置
def g:GetVistaConfig(): dict<any>
  return config
enddef

# 立即初始化
call g:InitVista()

# vim: set ft=vim sw=2 ts=2 sts=2 et:
