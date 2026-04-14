vim9script
# ============================================================================
# 模块: Terminal / Floaterm
# 作者: mcge <mcgeq@outlook.com>
# 说明: 配置 Floaterm 浮动终端行为。
# ============================================================================

# 防止重复加载
if g:MarkModuleLoaded('floaterm')
  finish
endif

# 配置
var config = {
  enabled: true,
  shell: g:mcge_custom_shell,
  width: 0.8,
  height: 0.8,
  title: 'Floaterm',
  position: 'center',
  autofocus: 1,
  autoinsert: 1,
}

# 初始化 Floaterm
def g:InitFloaterm(user_config: dict<any> = {})
  config = g:ResolveModuleConfig('floaterm', config, user_config)

  if g:ModuleIsDisabled(config, 'Floaterm')
    return
  endif

  # 设置 shell 和窗口外观
  g:ApplyGlobalVars({
    floaterm_shell: config.shell,
    floaterm_width: config.width,
    floaterm_height: config.height,
    floaterm_title: config.title,
    floaterm_position: config.position,
    floaterm_autofocus: config.autofocus,
    floaterm_autoinsert: config.autoinsert,
  })

  call g:ErrDebug('Floaterm initialized')
enddef

# 健康检查
def g:FloatermHealthCheck(): dict<any>
  return g:BuildManagedCommandModuleHealth('floaterm', 'Floaterm', config, 'FloatermNew', {
    shell: config.shell,
  })
enddef

# 获取配置
def g:GetFloatermConfig(): dict<any>
  return config
enddef

# 立即初始化
call g:InitFloaterm()

# vim: set ft=vim sw=2 ts=2 sts=2 et:
