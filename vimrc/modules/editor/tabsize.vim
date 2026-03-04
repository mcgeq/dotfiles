vim9script
# ============================================================================
# Editor 模块 - Tab 和缩进配置
# 作者：mcge <mcgeq@outlook.com>
# ============================================================================

# 防止重复加载
if exists('g:mcge_tabsize_loaded')
  finish
endif
g:mcge_tabsize_loaded = true

# 配置
var config = {
  enabled: true,
  expandtab: true,
  tabstop: 4,
  softtabstop: 4,
  shiftwidth: 4,
  filetypes: {
    rust: { tabstop: 4, shiftwidth: 4 },
    python: { tabstop: 4, shiftwidth: 4 },
    javascript: { tabstop: 2, shiftwidth: 2 },
    javascriptreact: { tabstop: 2, shiftwidth: 2 },
    typescript: { tabstop: 2, shiftwidth: 2 },
    typescriptreact: { tabstop: 2, shiftwidth: 2 },
    csharp: { tabstop: 4, shiftwidth: 4 },
    html: { tabstop: 2, shiftwidth: 2 },
  },
}

# 初始化 Tab 配置
def g:InitTabSize(user_config: dict<any> = {})
  # 合并用户配置
  extend(config, user_config)

  if !config.enabled
    call g:ErrDebug('TabSize is disabled')
    return
  endif

  # 基础设置（使用 execute 设置选项）
  execute 'set expandtab'
  execute 'set tabstop=' .. config.tabstop
  execute 'set softtabstop=' .. config.softtabstop
  execute 'set shiftwidth=' .. config.shiftwidth

  # 按文件类型设置
  SetupFileTypeAutocmds()

  call g:ErrDebug('TabSize initialized')
enddef

# 设置文件类型自动命令
def SetupFileTypeAutocmds()
  augroup mcge_tabsize
    autocmd!
    for [ft, opts] in items(config.filetypes)
      execute 'autocmd FileType ' .. ft .. ' setlocal tabstop=' .. opts.tabstop .. ' shiftwidth=' .. opts.shiftwidth
    endfor
  augroup END
enddef

# 健康检查
def g:TabSizeHealthCheck(): dict<any>
  return {
    name: 'TabSize',
    enabled: config.enabled,
    tabstop: config.tabstop,
    shiftwidth: config.shiftwidth,
    filetypes_configured: len(config.filetypes),
    status: config.enabled ? 'running' : 'disabled',
  }
enddef

# 获取配置
def g:GetTabSizeConfig(): dict<any>
  return config
enddef

# 立即初始化
call g:InitTabSize()

# vim: set ft=vim sw=2 ts=2 sts=2 et:
