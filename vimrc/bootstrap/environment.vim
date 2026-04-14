vim9script
# ============================================================================
# 组件: Bootstrap / Environment
# 作者: mcge <mcgeq@outlook.com>
# 说明: 启动期环境检测、依赖检查与目录初始化。
# ============================================================================

# ----------------------------------------------------------------------------
# 内部辅助
# ----------------------------------------------------------------------------
def CheckVimVersion(): bool
  if has('nvim')
    return true
  else
    if v:version < 900
      echohl ErrorMsg
      echo 'Error: Vim version too old. Required: >= 9.0'
      echohl None
      return false
    endif
  endif
  return true
enddef

# 检测必需依赖
def CheckDependencies(): list<string>
  const required = ['git', 'rg', 'fd']
  var missing: list<string> = []
  
  for cmd in required
    if !executable(cmd)
      add(missing, cmd)
    endif
  endfor
  
  return missing
enddef

# 创建必需的目录
def CreateDirectories()
  const dirs = [
    g:mcge_customvimrcdir .. '/data',
    g:mcge_customvimrcdir .. '/cache',
    g:mcge_customvimrcdir .. '/tmp',
  ]
  
  for dir in dirs
    if !isdirectory(dir)
      call mkdir(dir, 'p')
    endif
  endfor
enddef

# ----------------------------------------------------------------------------
# 公共接口
# ----------------------------------------------------------------------------
def g:EnvironmentInitialize()
  # 检查版本
  if !CheckVimVersion()
    return
  endif
  
  # 检查依赖
  const missing = CheckDependencies()
  if !empty(missing)
    echohl WarningMsg
    echo 'Warning: Missing dependencies: ' .. join(missing, ', ')
    echohl None
  endif
  
  # 创建目录
  CreateDirectories()
enddef

# vim: set ft=vim sw=2 ts=2 sts=2 et:
