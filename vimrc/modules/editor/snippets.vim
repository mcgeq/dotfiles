vim9script
# ============================================================================
# Editor 模块 - 代码片段 (UltiSnips)
# 作者：mcge <mcgeq@outlook.com>
# ============================================================================

# 防止重复加载
if exists('g:mcge_snippets_loaded')
  finish
endif
g:mcge_snippets_loaded = true

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
  # 合并用户配置
  extend(config, user_config)

  if !config.enabled
    call g:ErrDebug('UltiSnips is disabled')
    return
  endif

  # 设置片段目录
  g:UltiSnipsSnippetDirectories = config.snippet_directories

  # 设置触发器
  g:UltiSnipsExpandTrigger = config.expand_trigger
  g:UltiSnipsListSnippets = config.list_trigger
  g:UltiSnipsJumpForwardTrigger = config.jump_forward_trigger
  g:UltiSnipsJumpBackwardTrigger = config.jump_backward_trigger

  call g:ErrDebug('UltiSnips initialized')
enddef

# 健康检查
def g:UltiSnipsHealthCheck(): dict<any>
  var has_python = has('python3')

  return {
    name: 'UltiSnips',
    available: has_python,
    enabled: config.enabled,
    python3_available: has_python,
    snippet_dirs: len(config.snippet_directories),
    status: config.enabled && has_python ? 'running' : 'disabled',
  }
enddef

# 获取配置
def g:GetUltiSnipsConfig(): dict<any>
  return config
enddef

# 立即初始化
call g:InitUltiSnips()

# vim: set ft=vim sw=2 ts=2 sts=2 et:
