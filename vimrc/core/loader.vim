vim9script
# ============================================================================
# 组件: Core / Loader
# 作者: mcge <mcgeq@outlook.com>
# 说明: 提供模块加载、懒加载和加载统计能力。
# 注意: 本文件通过 source 加载，导出全局函数 g:*。
# ============================================================================

# ----------------------------------------------------------------------------
# 运行时状态
# ----------------------------------------------------------------------------

var loaded_modules: dict<bool> = {}
var failed_modules: list<string> = []
var load_times: dict<float> = {}  # 改为 float 类型，因为存储的是浮点数时间

# ----------------------------------------------------------------------------
# 内部辅助
# ----------------------------------------------------------------------------

# 生成稳定且唯一的模块 ID，避免不同目录下同名文件冲突
def NormalizeModuleId(filepath: string, module_name: string): string
  if !empty(module_name)
    return module_name
  endif

  const absolute_path = substitute(fnamemodify(filepath, ':p'), '\\', '/', 'g')
  const config_root = substitute(fnamemodify(g:mcge_customvimrcdir, ':p'), '\\', '/', 'g')
  var relative_path = absolute_path

  if stridx(absolute_path, config_root) == 0
    relative_path = absolute_path[len(config_root) : ]
  endif

  relative_path = substitute(relative_path, '^/', '', '')
  return substitute(relative_path, '\.vim$', '', '')
enddef

def SanitizeIdentifier(text: string): string
  var value = substitute(text, '[^A-Za-z0-9_]', '_', 'g')
  value = substitute(value, '_\+', '_', 'g')

  if empty(value)
    return 'unnamed'
  endif

  if value =~ '^\d'
    value = 'id_' .. value
  endif

  return value
enddef

def BuildLoadFileCall(filepath: string, module_name: string): string
  return $'call g:LoadFile({string(filepath)}, {string(module_name)})'
enddef

def RegisterLazyAutocmd(events: string, pattern: string, filepath: string, module_name: string)
  const group_name = 'mcge_lazyload_' .. SanitizeIdentifier(module_name .. '_' .. events .. '_' .. pattern)
  const load_call = BuildLoadFileCall(filepath, module_name)

  execute 'augroup ' .. group_name
    autocmd!
    execute $'autocmd {events} {pattern} ++once {load_call}'
  augroup END
enddef

# ----------------------------------------------------------------------------
# 核心加载函数
# ----------------------------------------------------------------------------

# 加载单个文件
def g:LoadFile(filepath: string, module_name: string = ''): bool
  if !filereadable(filepath)
    g:ErrWarn($'File not readable: {filepath}')
    return false
  endif
  
  const name = NormalizeModuleId(filepath, module_name)
  
  # 检查是否已加载
  if get(loaded_modules, name, false)
    g:ErrDebug($'Module {name} already loaded')
    return true
  endif
  
  # 开始计时（使用 var 而非 const，因为 float 类型）
  var start_time = reltime()->reltimefloat() * 1000
  
  # 加载文件
  try
    execute 'source ' .. fnameescape(filepath)
    loaded_modules[name] = true
    
    # 计算加载时间（使用 var 而非 const）
    var elapsed = (reltime()->reltimefloat() * 1000) - start_time
    load_times[name] = elapsed
    
    # 性能警告
    if elapsed > 50
      g:ErrWarn($'Module {name} took {printf("%.2f", elapsed)}ms to load')
    else
      g:ErrDebug($'Module {name} loaded in {printf("%.2f", elapsed)}ms')
    endif
    
    return true
  catch
    g:ErrError($'Failed to load {filepath}: {v:exception}')
    if index(failed_modules, name) < 0
      add(failed_modules, name)
    endif
    return false
  endtry
enddef

# 加载目录下的所有文件
def g:LoadDirectory(dir_path: string, recursive: bool = false): number
  if !isdirectory(dir_path)
    g:ErrWarn($'Directory not found: {dir_path}')
    return 0
  endif
  
  const files = recursive ? g:GetAllVimFiles(dir_path) : glob(dir_path .. '/*.vim', 0, 1)
  
  var loaded_count = 0
  for file in sort(files)
    if g:LoadFile(file)
      ++loaded_count
    endif
  endfor
  
  g:ErrDebug($'Loaded {loaded_count}/{len(files)} files from {dir_path}')  # 改为调试级别
  
  return loaded_count
enddef

# 条件加载
def g:LoadIf(condition: bool, filepath: string, module_name: string = ''): bool
  if !condition
    g:ErrDebug($'Condition not met for {filepath}')
    return false
  endif
  return g:LoadFile(filepath, module_name)
enddef

# 懒加载（事件触发）
def g:LazyLoad(filepath: string, events: any, module_name: string = '')
  const name = NormalizeModuleId(filepath, module_name)
  
  if type(events) == v:t_list
    RegisterLazyAutocmd(join(events, ','), '*', filepath, name)
  else
    RegisterLazyAutocmd(events, '*', filepath, name)
  endif
  
  g:ErrDebug($'Lazy load registered for {name}: {events}')
enddef

# 延迟加载（定时器）
def g:DeferLoad(filepath: string, delay: number = 100, module_name: string = '')
  timer_start(delay, (_) => {
    g:LoadFile(filepath, module_name)
  })
enddef

# 按文件类型懒加载
def g:LoadForFiletype(filetypes: any, filepath: string, module_name: string = '')
  const name = NormalizeModuleId(filepath, module_name)

  if type(filetypes) == v:t_list
    RegisterLazyAutocmd('FileType', join(filetypes, ','), filepath, name)
  else
    RegisterLazyAutocmd('FileType', string(filetypes)->trim("'"), filepath, name)
  endif

  g:ErrDebug($'FileType lazy load registered for {name}: {filetypes}')
enddef

# 在插入模式时懒加载
def g:LoadOnInsert(filepath: string, module_name: string = '')
  g:LazyLoad(filepath, 'InsertEnter', module_name)
enddef

# 在读取文件时懒加载
def g:LoadOnBufRead(filepath: string, module_name: string = '')
  g:LazyLoad(filepath, 'BufRead', module_name)
enddef

# 在命令执行前懒加载
def g:LoadBeforeCommand(command: string, filepath: string, module_name: string = '')
  const name = NormalizeModuleId(filepath, module_name)
  const load_call = BuildLoadFileCall(filepath, name)
  execute $'command! -nargs=* {command} ++once {load_call} | {command} <args>'
enddef

# 条件懒加载（满足条件时立即加载，否则监听事件）
def g:LoadWhen(condition: bool, filepath: string, events: any = [], module_name: string = '')
  if condition
    g:LoadFile(filepath, module_name)
  elseif !empty(events)
    g:LazyLoad(filepath, events, module_name)
  endif
enddef

# ----------------------------------------------------------------------------
# 查询函数
# ----------------------------------------------------------------------------

# 获取失败的模块
def g:GetFailedModules(): list<string>
  return failed_modules
enddef

# 检查模块是否已加载
def g:IsModuleLoaded(name: string): bool
  return get(loaded_modules, name, false)
enddef

# 获取加载统计
def g:GetLoadStats(): dict<any>
  var total_time = 0.0
  for [_, time] in items(load_times)
    total_time += time
  endfor

  const loaded = len(loaded_modules)
  const failed = len(failed_modules)
  
  return {
    total: loaded + failed,
    loaded: loaded,
    failed: failed,
    total_time: total_time,
  }
enddef

# 打印加载报告
def g:PrintReport()
  const stats = g:GetLoadStats()
  
  echo '=========================================='
  echo 'Module Loading Report'
  echo '=========================================='
  echo $'Total modules: {stats.total}'
  echo $'Failed: {stats.failed}'
  echo $'Total load time: {printf("%.2f", stats.total_time)}ms'
  
  if !empty(failed_modules)
    echo ''
    echo 'Failed modules:'
    for name in failed_modules
      echo $'  ✗ {name}'
    endfor
  endif
  
  echo '=========================================='
enddef

# ----------------------------------------------------------------------------
# 命令定义
# ----------------------------------------------------------------------------

command! VimrcLoadReport call g:PrintReport()

# vim: set ft=vim sw=2 ts=2 sts=2 et:
