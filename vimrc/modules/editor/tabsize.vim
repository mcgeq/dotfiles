vim9script
# ============================================================================
# 模块: Editor / TabSize
# 作者: mcge <mcgeq@outlook.com>
# 说明: 配置全局与文件类型缩进参数。
# ============================================================================

# 防止重复加载
if g:MarkModuleLoaded('tabsize')
  finish
endif

# 配置
var config = {
  enabled: true,
  expandtab: true,
  tabstop: 4,
  softtabstop: 4,
  shiftwidth: 4,
  filetypes: {
    rust: { tabstop: 4, softtabstop: 4, shiftwidth: 4 },
    python: { tabstop: 4, softtabstop: 4, shiftwidth: 4 },
    javascript: { tabstop: 2, softtabstop: 2, shiftwidth: 2 },
    javascriptreact: { tabstop: 2, softtabstop: 2, shiftwidth: 2 },
    typescript: { tabstop: 2, softtabstop: 2, shiftwidth: 2 },
    typescriptreact: { tabstop: 2, softtabstop: 2, shiftwidth: 2 },
    csharp: { tabstop: 4, softtabstop: 4, shiftwidth: 4 },
    html: { tabstop: 2, softtabstop: 2, shiftwidth: 2 },
    css: { tabstop: 2, softtabstop: 2, shiftwidth: 2 },
    scss: { tabstop: 2, softtabstop: 2, shiftwidth: 2 },
  },
}

def ApplyGlobalTabOptions()
  &g:expandtab = config.expandtab
  &g:tabstop = config.tabstop
  &g:softtabstop = config.softtabstop
  &g:shiftwidth = config.shiftwidth
enddef

def ApplyLocalTabOptions(options: dict<any>)
  &l:tabstop = options.tabstop
  &l:softtabstop = get(options, 'softtabstop', options.shiftwidth)
  &l:shiftwidth = options.shiftwidth
enddef

def g:ApplyTabSizeForFiletype(filetype: string)
  if !has_key(config.filetypes, filetype)
    return
  endif

  ApplyLocalTabOptions(config.filetypes[filetype])
enddef

# 初始化 Tab 配置
def g:InitTabSize(user_config: dict<any> = {})
  config = g:ResolveModuleConfigDeep('tabsize', config, user_config)

  if g:ModuleIsDisabled(config, 'TabSize')
    return
  endif

  # 基础设置
  ApplyGlobalTabOptions()

  # 按文件类型设置
  RegisterFileTypeAutocmds()

  call g:ErrDebug('TabSize initialized')
enddef

# 设置文件类型自动命令
def RegisterFileTypeAutocmds()
  augroup mcge_tabsize
    autocmd!
    autocmd FileType * call g:ApplyTabSizeForFiletype(expand('<amatch>'))
  augroup END
enddef

# 健康检查
def g:TabSizeHealthCheck(): dict<any>
  return g:BuildManagedModuleHealth('tabsize', 'TabSize', config, {
    tabstop: config.tabstop,
    shiftwidth: config.shiftwidth,
    filetypes_configured: len(config.filetypes),
  })
enddef

# 获取配置
def g:GetTabSizeConfig(): dict<any>
  return config
enddef

# 立即初始化
call g:InitTabSize()

# vim: set ft=vim sw=2 ts=2 sts=2 et:
