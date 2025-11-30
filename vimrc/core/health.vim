vim9script
# ============================================================================
# 健康检查系统
# 作者: mcge <mcgeq@outlook.com>
# 说明: 检查配置的健康状态，包括环境、依赖、插件等
# ============================================================================

# 检查结果类型
type CheckResult = dict<any>

var all_checks: list<CheckResult> = []

# ----------------------------------------------------------------------------
# 辅助函数
# ----------------------------------------------------------------------------

def OK(name: string, message: string): CheckResult
  return {name: name, status: 'ok', message: message, details: []}
enddef

def Warn(name: string, message: string, details: list<string> = []): CheckResult
  return {name: name, status: 'warning', message: message, details: details}
enddef

def Error(name: string, message: string, details: list<string> = []): CheckResult
  return {name: name, status: 'error', message: message, details: details}
enddef

# ----------------------------------------------------------------------------
# 各项检查函数
# ----------------------------------------------------------------------------

def CheckVimVersion(): CheckResult
  if has('nvim')
    const version = api_info().version
    const ver_str = $'{version.major}.{version.minor}.{version.patch}'
    return OK('vim_version', $'Neovim {ver_str}')
  else
    if v:version < 900
      return Error('vim_version', $'Vim version too old: {v:version}', ['Required: >= 9.0'])
    endif
    return OK('vim_version', $'Vim {v:version}')
  endif
enddef

def CheckExecutables(): CheckResult
  const required = {
    git: 'Version control',
    rg: 'Fast text search',
    fd: 'Fast file search',
  }
  
  const optional = {
    bat: 'Better cat',
    node: 'JavaScript runtime',
    npm: 'Node package manager',
  }
  
  var missing_required: list<string> = []
  var missing_optional: list<string> = []
  
  for [cmd, desc] in items(required)
    if !executable(cmd)
      add(missing_required, $'{cmd} - {desc}')
    endif
  endfor
  
  for [cmd, desc] in items(optional)
    if !executable(cmd)
      add(missing_optional, $'{cmd} - {desc}')
    endif
  endfor
  
  if !empty(missing_required)
    return Error('executables', 'Missing required executables', missing_required)
  endif
  
  if !empty(missing_optional)
    return Warn('executables', 'Missing optional executables', missing_optional)
  endif
  
  return OK('executables', 'All required executables found')
enddef

def CheckPython(): CheckResult
  if !has('python3')
    return Warn('python', 'Python 3 support not available')
  endif
  
  try
    const py_version = py3eval('sys.version.split()[0]')
    return OK('python', $'Python 3 available: {py_version}')
  catch
    return Error('python', 'Python 3 error', [v:exception])
  endtry
enddef

def CheckDirectories(): CheckResult
  const required_dirs = [
    g:BOOTSTRAP_DIR,
    g:CORE_DIR,
    g:MODULES_DIR,
    g:DATA_DIR,
  ]
  
  var missing: list<string> = []
  
  for dir in required_dirs
    if !isdirectory(dir)
      add(missing, dir)
    endif
  endfor
  
  if !empty(missing)
    return Error('directories', 'Missing required directories', missing)
  endif
  
  return OK('directories', 'All required directories present')
enddef

def CheckCoreFiles(): CheckResult
  const required_files = [
    g:BOOTSTRAP_DIR .. '/constants.vim',
    g:BOOTSTRAP_DIR .. '/settings.vim',
    g:CORE_DIR .. '/error_handler.vim',
    g:CORE_DIR .. '/utils.vim',
    g:CORE_DIR .. '/loader.vim',
  ]
  
  var missing: list<string> = []
  
  for file in required_files
    if !filereadable(file)
      add(missing, file)
    endif
  endfor
  
  if !empty(missing)
    return Error('core_files', 'Missing required core files', missing)
  endif
  
  return OK('core_files', 'All core files present')
enddef

def CheckModuleLoading(): CheckResult
  if !exists('*g:GetLoadStats')
    return Warn('module_loading', 'Loader not initialized')
  endif
  
  const stats = g:GetLoadStats()
  
  if stats.failed > 0
    const failed = g:GetFailedModules()
    return Error('module_loading', $'{stats.failed} module(s) failed to load', failed)
  endif
  
  return OK('module_loading', $'{stats.loaded}/{stats.total} modules loaded')
enddef

def CheckPerformance(): CheckResult
  if !exists('g:mcge_startup_time')
    return Warn('performance', 'Startup time not recorded')
  endif
  
  const time = g:mcge_startup_time
  
  if time > 300
    return Error('performance', $'Startup time too slow: {printf("%.2f", time)}ms', ['Threshold: 300ms'])
  elseif time > 150
    return Warn('performance', $'Startup time warning: {printf("%.2f", time)}ms', ['Threshold: 150ms'])
  endif
  
  return OK('performance', $'Startup time: {printf("%.2f", time)}ms')
enddef

def CheckCoc(): CheckResult
  if !exists('g:did_coc_loaded')
    return Warn('coc', 'CoC not loaded yet')
  endif
  
  try
    const extensions = get(g:, 'coc_global_extensions', [])
    return OK('coc', $'CoC loaded with {len(extensions)} extensions')
  catch
    return Error('coc', 'CoC error', [v:exception])
  endtry
enddef

def CheckEncoding(): CheckResult
  if &encoding != 'utf-8'
    return Warn('encoding', $'Encoding is not UTF-8: {&encoding}')
  endif
  return OK('encoding', 'Encoding: UTF-8')
enddef

# ----------------------------------------------------------------------------
# 主检查函数
# ----------------------------------------------------------------------------

export def g:RunHealthCheck(): list<CheckResult>
  all_checks = []
  
  const checks = [
    CheckVimVersion,
    CheckDirectories,
    CheckCoreFiles,
    CheckExecutables,
    CheckPython,
    CheckEncoding,
    CheckModuleLoading,
    CheckPerformance,
    CheckCoc,
  ]
  
  for Check in checks
    try
      add(all_checks, Check())
    catch
      add(all_checks, Error('check_error', $'Check failed: {v:exception}', []))
    endtry
  endfor
  
  return all_checks
enddef

export def g:PrintHealthReport()
  const results = g:RunHealthCheck()
  
  echo ''
  echohl Title
  echo '╔════════════════════════════════════════╗'
  echo '║    Vim Configuration Health Check     ║'
  echo '╚════════════════════════════════════════╝'
  echohl None
  echo ''
  
  var ok_count = 0
  var warn_count = 0
  var error_count = 0
  
  for result in results
    if result.status == 'ok'
      echohl String
      const icon = '✓'
      ++ok_count
    elseif result.status == 'warning'
      echohl WarningMsg
      const icon = '⚠'
      ++warn_count
    else
      echohl ErrorMsg
      const icon = '✗'
      ++error_count
    endif
    
    echo $'  {icon} {result.name}: {result.message}'
    echohl None
    
    if !empty(result.details)
      for detail in result.details
        echo $'      - {detail}'
      endfor
    endif
  endfor
  
  echo ''
  echo '─────────────────────────────────────────'
  echohl String
  echo $'  ✓ OK: {ok_count}'
  echohl WarningMsg
  echo $'  ⚠ Warnings: {warn_count}'
  echohl ErrorMsg
  echo $'  ✗ Errors: {error_count}'
  echohl None
  echo '─────────────────────────────────────────'
  echo ''
enddef

export def g:QuickHealthCheck(): list<string>
  var issues: list<string> = []
  
  const ver_result = CheckVimVersion()
  if ver_result.status == 'error'
    add(issues, ver_result.message)
  endif
  
  const files_result = CheckCoreFiles()
  if files_result.status == 'error'
    add(issues, files_result.message)
  endif
  
  if exists('*g:GetFailedModules')
    const failed = g:GetFailedModules()
    if !empty(failed)
      add(issues, $'{len(failed)} modules failed to load')
    endif
  endif
  
  return issues
enddef

# 命令定义
command! VimrcHealthCheck call g:PrintHealthReport()
command! -nargs=0 CheckHealth call g:PrintHealthReport()

# vim: set ft=vim sw=2 ts=2 sts=2 et:
