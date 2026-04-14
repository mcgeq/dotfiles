vim9script
# ============================================================================
# 模块: Editor / Snippets
# 作者: mcge <mcgeq@outlook.com>
# 说明: 配置 UltiSnips 片段目录和触发键。
# ============================================================================

# 防止重复加载
if g:MarkModuleLoaded('snippets')
  finish
endif

# 配置
var config = {
  enabled: true,
  snippet_directories: [
    'UltiSnips',
    'vim-snippets/snippets',
    g:mcge_customvimrcdir .. '/snippets',
  ],
  expand_trigger: '<tab>',
  list_trigger: '<c-tab>',
  jump_forward_trigger: '<c-b>',
  jump_backward_trigger: '<c-z>',
}

# 初始化 UltiSnips
def g:InitUltiSnips(user_config: dict<any> = {})
  config = g:ResolveModuleConfig('snippets', config, user_config)

  if g:ModuleIsDisabled(config, 'UltiSnips')
    return
  endif

  # 设置片段目录和触发器
  g:ApplyGlobalVars({
    UltiSnipsSnippetDirectories: config.snippet_directories,
    UltiSnipsExpandTrigger: config.expand_trigger,
    UltiSnipsListSnippets: config.list_trigger,
    UltiSnipsJumpForwardTrigger: config.jump_forward_trigger,
    UltiSnipsJumpBackwardTrigger: config.jump_backward_trigger,
  })

  call g:ErrDebug('UltiSnips initialized')
enddef

# 健康检查
def g:UltiSnipsHealthCheck(): dict<any>
  var has_python = has('python3')

  return g:BuildManagedModuleHealth('snippets', 'UltiSnips', config, {
    available: has_python,
    python3_available: has_python,
    snippet_dirs: len(config.snippet_directories),
    status: has_python ? (config.enabled ? 'running' : 'disabled') : 'not loaded',
  })
enddef

# 获取配置
def g:GetUltiSnipsConfig(): dict<any>
  return config
enddef

# 立即初始化
call g:InitUltiSnips()

# vim: set ft=vim sw=2 ts=2 sts=2 et:
