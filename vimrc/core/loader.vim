vim9script
# ============================================================================
# 模块加载器
# 作者: mcge <mcgeq@outlook.com>
# 注意: 本文件通过 source 加载，使用全局函数 g:
# ============================================================================

# 模块状态
var loaded_modules: dict<bool> = {}
var failed_modules: list<string> = []
var load_times: dict<float> = {}  # 改为 float 类型，因为存储的是浮点数时间

# ----------------------------------------------------------------------------
# 核心加载函数
# ----------------------------------------------------------------------------

# 加载单个文件
def g:LoadFile(filepath: string, module_name: string = ''): bool
  if !filereadable(filepath)
    g:ErrWarn($'File not readable: {filepath}')
    return false
  endif
  
  const name = empty(module_name) ? fnamemodify(filepath, ':t:r') : module_name
  
  # 检查是否已加载
  if get(loaded_modules, name, false)
    g:ErrDebug($'Module {name} already loaded')
    return true
  endif
  
  # 开始计时（使用 var 而非 const，因为 float 类型）
  var start_time = reltime()->reltimefloat() * 1000
  
  # 加载文件
  try
    execute 'source ' .. filepath
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
    add(failed_modules, name)
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
  const name = empty(module_name) ? fnamemodify(filepath, ':t:r') : module_name
  
  if type(events) == v:t_list
    const event_list = join(events, ',')
    execute $'autocmd {event_list} * ++once call g:LoadFile("{filepath}", "{name}")'
  else
    execute $'autocmd {events} * ++once call g:LoadFile("{filepath}", "{name}")'
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
  if type(filetypes) == v:t_list
    const ft_list = join(filetypes, ',')
    g:LazyLoad(filepath, $'FileType {ft_list}', module_name)
  else
    g:LazyLoad(filepath, $'FileType {filetypes}', module_name)
  endif
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
  const name = empty(module_name) ? fnamemodify(filepath, ':t:r') : module_name
  execute $'command! -nargs=* {command} ++once call g:LoadFile("{filepath}", "{name}") | {command} <args>'
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
  
  return {
    total: len(loaded_modules),
    loaded: len(loaded_modules),
    failed: len(failed_modules),
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

# 命令定义
command! VimrcLoadReport call g:PrintReport()

# vim: set ft=vim sw=2 ts=2 sts=2 et:
