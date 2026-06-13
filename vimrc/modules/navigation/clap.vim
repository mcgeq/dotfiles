vim9script
# ============================================================================
# 模块: Navigation / Clap
# 作者: mcge <mcgeq@outlook.com>
# 说明: 配置 Clap 搜索界面与 provider 行为。
# ============================================================================

# 防止重复加载
if g:MarkModuleLoaded('clap')
  finish
endif

# 配置
var config = {
  enabled: true,
  theme: 'material_design_dark',
  enable_icon: 1,
  layout: {
    relative: 'editor',
    width: '85%',
    height: '50%',
    row: '15%',
    col: '7%',
  },
  enable_background_shadow: 1,
  prompt_format: '%provider_id%>>',
  enable_indicator: 1,
  preview_size: 10,
  preview_direction: 'UD',
  grep_delay: 200,
  grep_blink: [2, 100],
  cache_directory: '~/.cache/clap',
  provider_history_size: 50,
  insert_mode_only: 0,
  force_python: 0,
}

# 初始化 Clap
def g:InitClap(user_config: dict<any> = {})
  config = g:ResolveModuleConfigDeep('clap', config, user_config)

  if g:ModuleIsDisabled(config, 'Clap')
    return
  endif

  # 外观、预览和性能
  g:ApplyGlobalVars({
    clap_theme: config.theme,
    clap_enable_icon: config.enable_icon,
    clap_layout: config.layout,
    clap_enable_background_shadow: config.enable_background_shadow,
    clap_prompt_format: config.prompt_format,
    clap_enable_indicator: config.enable_indicator,
    clap_preview_size: config.preview_size,
    clap_preview_direction: config.preview_direction,
    clap_provider_grep_delay: config.grep_delay,
    clap_provider_grep_blink: config.grep_blink,
    clap_cache_directory: expand(config.cache_directory),
    clap_provider_history_size: config.provider_history_size,
    clap_insert_mode_only: config.insert_mode_only,
    clap_force_python: config.force_python,
  })

  # 设置搜索工具
  ConfigureProviders()

  call g:ErrDebug('Clap initialized')
enddef

# 设置 Provider
def ConfigureProviders()
  # 使用 ripgrep 和 fd（性能最佳）
  if executable('rg')
    g:clap_provider_grep_executable = 'rg'
    g:clap_provider_grep_opts = '--vimgrep --smart-case --hidden'
  endif

  if executable('fd')
    g:clap_provider_files_executable = 'fd'
    g:clap_provider_files_opts = '--type f --hidden --follow --exclude .git --exclude node_modules'
  endif
enddef

# 健康检查
def g:ClapHealthCheck(): dict<any>
  var rg_available = executable('rg')
  var fd_available = executable('fd')

  return g:BuildManagedCommandModuleHealth('clap', 'Clap', config, 'Clap', {
    theme: config.theme,
    rg_available: rg_available,
    fd_available: fd_available,
  })
enddef

# 获取配置
def g:GetClapConfig(): dict<any>
  return config
enddef

# 立即初始化
call g:InitClap()

# vim: set ft=vim sw=2 ts=2 sts=2 et:
