vim9script
# ============================================================================
# 错误处理器
# 作者: mcge <mcgeq@outlook.com>
# ============================================================================

var error_log: list<string> = []
var warning_log: list<string> = []

# 日志配置
var config = {
  log_file: expand(g:mcge_customvimrcdir .. '/data/errors.log'),
  show_notifications: true,
  verbose: false,
}

# 格式化消息
def FormatMessage(level: string, msg: string): string
  const timestamp = strftime('%Y-%m-%d %H:%M:%S')
  return $'[{timestamp}] [{level}] {msg}'
enddef

# 通用日志函数
def Log(level: string, msg: string)
  const formatted = FormatMessage(level, msg)
  
  if level == 'ERROR' || level == 'FATAL'
    add(error_log, formatted)
  elseif level == 'WARN'
    add(warning_log, formatted)
  endif
  
  # 写入日志文件
  if filewritable(config.log_file) || !filereadable(config.log_file)
    writefile([formatted], config.log_file, 'a')
  endif
  
  # 显示通知
  if config.show_notifications
    if level == 'ERROR' || level == 'FATAL'
      echohl ErrorMsg
    elseif level == 'WARN'
      echohl WarningMsg
    else
      echohl None
    endif
    
    echo msg
    echohl None
  endif
enddef

# 导出函数（全局函数）
def g:ErrDebug(msg: string)
  if config.verbose
    Log('DEBUG', msg)
  endif
enddef

def g:ErrInfo(msg: string)
  Log('INFO', msg)
enddef

def g:ErrWarn(msg: string)
  Log('WARN', msg)
enddef

def g:ErrError(msg: string)
  Log('ERROR', msg)
enddef

def g:ErrFatal(msg: string)
  Log('FATAL', msg)
  throw 'FATAL: ' .. msg
enddef

def g:ErrGetErrors(): list<string>
  return error_log
enddef

def g:ErrGetWarnings(): list<string>
  return warning_log
enddef

def g:ErrClearLogs()
  error_log = []
  warning_log = []
enddef

# vim: set ft=vim sw=2 ts=2 sts=2 et:
