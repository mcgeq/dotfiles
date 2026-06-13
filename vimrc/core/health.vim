vim9script
# ============================================================================
# 组件: Core / Health
# 作者: mcge <mcgeq@outlook.com>
# 说明: 汇总环境、核心文件和模块 health 状态。
# ============================================================================

# ----------------------------------------------------------------------------
# 运行时状态
# ----------------------------------------------------------------------------

# 检查结果类型
type CheckResult = dict<any>

var all_checks: list<CheckResult> = []

# ----------------------------------------------------------------------------
# 内部辅助
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

def NormalizeCheckName(name: string): string
  return substitute(tolower(name), '[^0-9a-z]\+', '_', 'g')
enddef

def ValueToDetailText(value: any): string
  if type(value) == v:t_bool
    return value ? 'true' : 'false'
  endif
  if type(value) == v:t_list
    return string(value)
  endif
  if type(value) == v:t_dict
    return string(value)
  endif
  return string(value)
enddef

def JoinDetailItems(values: list<any>): string
  if empty(values)
    return '(none)'
  endif
  return join(mapnew(copy(values), (_, value) => string(value)), ', ')
enddef

def ModuleExtraKeys(result: dict<any>): list<string>
  const excluded = [
    'name',
    'status',
    'enabled',
    'available',
    'module_id',
    'config_keys',
    'override_keys',
    'has_overrides',
  ]
  return sort(filter(keys(result), (_, key) => index(excluded, key) < 0))
enddef

def ManagedModuleDetails(result: dict<any>): list<string>
  var details: list<string> = []
  const module_id = get(result, 'module_id', '')
  const config_keys = get(result, 'config_keys', [])
  const override_keys = get(result, 'override_keys', [])
  const has_overrides = get(result, 'has_overrides', false)

  if module_id != ''
    add(details, $'module_id: {module_id}')
  endif

  add(details, $'enabled: {ValueToDetailText(get(result, "enabled", true))}')
  add(details, $'available: {ValueToDetailText(get(result, "available", true))}')
  add(details, $'overrides: {has_overrides ? $"yes ({JoinDetailItems(override_keys)})" : "no"}')
  add(details, $'config keys ({len(config_keys)}): {JoinDetailItems(config_keys)}')

  for key in ModuleExtraKeys(result)
    add(details, $'{key}: {ValueToDetailText(result[key])}')
  endfor

  return details
enddef

def ModuleResultDetails(result: dict<any>): list<string>
  if has_key(result, 'module_id')
    return ManagedModuleDetails(result)
  endif

  var details: list<string> = []
  for key in sort(keys(result))
    const value = result[key]
    if index(['name', 'status'], key) >= 0
      continue
    endif
    add(details, $'{key}: {ValueToDetailText(value)}')
  endfor
  return details
enddef

def ModuleResultStatus(result: dict<any>): string
  const status = tolower(string(get(result, 'status', '')))
  const enabled = get(result, 'enabled', true)
  const available = get(result, 'available', true)

  if status =~# '^\(error\|failed\)$'
    return 'error'
  endif

  if status ==# 'not loaded'
    return 'warning'
  endif

  if enabled == false || status ==# 'disabled'
    return 'ok'
  endif

  if available == false
    return 'warning'
  endif

  return 'ok'
enddef

def ModuleResultMessage(result: dict<any>): string
  const status = tolower(string(get(result, 'status', 'ok')))
  const name = get(result, 'name', 'module')

  if status ==# 'running'
    return $'{name} running'
  endif
  if status ==# 'disabled'
    return $'{name} disabled'
  endif
  if status ==# 'not loaded'
    return $'{name} not loaded'
  endif
  if status ==# 'ok'
    return $'{name} healthy'
  endif

  return $'{name} status: {status}'
enddef

def BuildModuleCheckResult(result: dict<any>): CheckResult
  const module_name = get(result, 'name', 'module')
  const check_name = 'module_' .. NormalizeCheckName(module_name)
  const message = ModuleResultMessage(result)
  const details = ModuleResultDetails(result)
  const status = ModuleResultStatus(result)

  if status ==# 'error'
    return Error(check_name, message, details)
  endif
  if status ==# 'warning'
    return Warn(check_name, message, details)
  endif
  return {
    name: check_name,
    kind: 'module',
    status: 'ok',
    message: message,
    details: details,
    meta: deepcopy(result),
  }
enddef

def ResultIcon(result: CheckResult): string
  if result.status == 'ok'
    return '✓'
  endif
  if result.status == 'warning'
    return '⚠'
  endif
  return '✗'
enddef

def ResultHighlight(result: CheckResult): string
  if result.status == 'ok'
    return 'String'
  endif
  if result.status == 'warning'
    return 'WarningMsg'
  endif
  return 'ErrorMsg'
enddef

def ModuleCheckLabel(result: CheckResult): string
  const meta = get(result, 'meta', {})
  const module_id = get(meta, 'module_id', '')
  const module_name = get(meta, 'name', result.name)
  return module_id != '' ? $'[{module_id}] {module_name}' : module_name
enddef

def ModuleCategory(result: CheckResult): string
  const meta = get(result, 'meta', {})
  const module_id = get(meta, 'module_id', '')
  const category_map = {
    appearance: 'UI',
    colorscheme: 'UI',
    airline: 'UI',
    startify: 'UI',
    whichkey: 'UI',
    coc: 'LSP',
    gutter: 'Git',
    clap: 'Navigation',
    vista: 'Navigation',
    floaterm: 'Terminal',
    tabsize: 'Editor',
    commenter: 'Editor',
    snippets: 'Editor',
    multi_cursor: 'Editor',
    match_pair: 'Editor',
    whitespace: 'Editor',
    tags: 'Editor',
  }

  return get(category_map, module_id, 'Other Modules')
enddef

def GroupModuleResults(results: list<CheckResult>): list<dict<any>>
  const category_order = [
    'UI',
    'LSP',
    'Git',
    'Navigation',
    'Terminal',
    'Editor',
    'Other Modules',
  ]
  var grouped: dict<list<CheckResult>> = {}

  for result in results
    const category = ModuleCategory(result)
    if !has_key(grouped, category)
      grouped[category] = []
    endif
    add(grouped[category], result)
  endfor

  var sections: list<dict<any>> = []

  for category in category_order
    if has_key(grouped, category)
      add(sections, {
        name: category,
        results: grouped[category],
      })
      remove(grouped, category)
    endif
  endfor

  for category in sort(keys(grouped))
    add(sections, {
      name: category,
      results: grouped[category],
    })
  endfor

  return sections
enddef

def FilterModuleResultsByStatus(results: list<CheckResult>, status: string): list<CheckResult>
  return filter(copy(results), (_, result) => result.status ==# status)
enddef

def PrintModuleSections(results: list<CheckResult>)
  for section in GroupModuleResults(results)
    echohl Comment
    echo $'    {section.name}'
    echohl None
    PrintCheckResults(section.results)
    echo ''
  endfor
enddef

def PrintCheckResults(results: list<CheckResult>)
  for result in results
    execute 'echohl ' .. ResultHighlight(result)

    if get(result, 'kind', '') ==# 'module'
      echo $'  {ResultIcon(result)} {ModuleCheckLabel(result)}: {result.message}'
    else
      echo $'  {ResultIcon(result)} {result.name}: {result.message}'
    endif

    echohl None

    if !empty(result.details)
      for detail in result.details
        echo $'      - {detail}'
      endfor
    endif
  endfor
enddef

def GetModuleHealthFunctions(): list<string>
  const excluded = [
    'g:RunHealthCheck',
    'g:QuickHealthCheck',
    'g:PrintHealthReport',
  ]
  var functions = getcompletion('g:', 'function')
  functions = filter(functions, (_, name) => name =~# '^g:.\+HealthCheck$' && index(excluded, name) < 0)
  return sort(functions)
enddef

def CollectModuleChecks(): list<CheckResult>
  var results: list<CheckResult> = []

  for name in GetModuleHealthFunctions()
    try
      const Check = function(name)
      const module_result = call(Check, [])
      if type(module_result) != v:t_dict
        add(results, Error('module_check_error', $'{name} returned unexpected type', [string(module_result)]))
        continue
      endif
      add(results, BuildModuleCheckResult(module_result))
    catch
      add(results, Error('module_check_error', $'Module check failed: {name}', [v:exception]))
    endtry
  endfor

  return results
enddef

# ----------------------------------------------------------------------------
# 检查项
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
    g:CORE_DIR .. '/keymap.vim',
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
# 公共接口
# ----------------------------------------------------------------------------

def g:RunHealthCheck(): list<CheckResult>
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

  extend(all_checks, CollectModuleChecks())
  
  return all_checks
enddef

def g:PrintHealthReport()
  const results = g:RunHealthCheck()
  const core_results = filter(copy(results), (_, result) => get(result, 'kind', '') !=# 'module')
  const module_results = filter(copy(results), (_, result) => get(result, 'kind', '') ==# 'module')
  const error_module_results = FilterModuleResultsByStatus(module_results, 'error')
  const warning_module_results = FilterModuleResultsByStatus(module_results, 'warning')
  const healthy_module_results = FilterModuleResultsByStatus(module_results, 'ok')
  
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
      ++ok_count
    elseif result.status == 'warning'
      ++warn_count
    else
      ++error_count
    endif
  endfor

  echohl Directory
  echo '  Core'
  echohl None
  PrintCheckResults(core_results)

  if !empty(module_results)
    echo ''
    echohl Directory
    echo '  Modules'
    echohl None

    if !empty(error_module_results)
      echohl ErrorMsg
      echo '    Errors'
      echohl None
      PrintModuleSections(error_module_results)
    endif

    if !empty(warning_module_results)
      echohl WarningMsg
      echo '    Warnings'
      echohl None
      PrintModuleSections(warning_module_results)
    endif

    if !empty(healthy_module_results)
      echohl String
      echo '    Healthy'
      echohl None
      PrintModuleSections(healthy_module_results)
    endif
  endif
  
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

def g:QuickHealthCheck(): list<string>
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

# ----------------------------------------------------------------------------
# 命令定义
# ----------------------------------------------------------------------------
command! VimrcHealthCheck call g:PrintHealthReport()
command! -nargs=0 CheckHealth call g:PrintHealthReport()

# vim: set ft=vim sw=2 ts=2 sts=2 et:
