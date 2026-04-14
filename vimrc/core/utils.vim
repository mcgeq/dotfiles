vim9script
# ============================================================================
# 组件: Core / Utils
# 作者: mcge <mcgeq@outlook.com>
# 说明: 提供通用文件、路径、字符串和 buffer 工具函数。
# 注意: 本文件通过 source 加载，导出全局函数 g:*。
# ============================================================================

# ----------------------------------------------------------------------------
# 文件与目录
# ----------------------------------------------------------------------------

def CurrentPathSeparator(): string
  return has('win32') || has('win64') ? nr2char(92) : '/'
enddef

def TrimTrailingPathSeparator(path: string): string
  var normalized = path
  while len(normalized) > 1 && (normalized[-1] ==# '/' || normalized[-1] ==# nr2char(92))
    normalized = normalized[: -2]
  endwhile
  return normalized
enddef

# 获取指定目录下的所有 .vim 文件（递归）
def g:GetAllVimFiles(directory: string): list<string>
  var files: list<string> = []
  CollectVimFilesRecursively(directory, files)
  return files
enddef

def CollectVimFilesRecursively(directory: string, files: list<string>)
  if !isdirectory(directory)
    return
  endif

  const directory_files: list<string> = glob(directory .. '/*', 0, 1)

  for item in directory_files
    if isdirectory(item)
      CollectVimFilesRecursively(item, files)
    elseif item =~# '\.vim$'
      add(files, item)
    endif
  endfor
enddef

def NormalizeComparablePath(path: string): string
  var normalized = TrimTrailingPathSeparator(g:NormalizePath(fnamemodify(path, ':p')))
  if has('win32') || has('win64')
    normalized = tolower(normalized)
  endif
  return normalized
enddef

def IsPathSeparator(char: string): bool
  return char ==# '/' || char ==# nr2char(92)
enddef

def IsPathWithin(parent: string, child: string): bool
  if child ==# parent
    return true
  endif

  if len(child) <= len(parent)
    return false
  endif

  return stridx(child, parent) == 0 && IsPathSeparator(child[len(parent)])
enddef

# ----------------------------------------------------------------------------
# CoC 辅助
# ----------------------------------------------------------------------------

# 检查光标前是否为空白
def g:CheckBackspace(): bool
  const colnum = col('.') - 1
  return colnum <= 0 || getline('.')[colnum - 1] =~# '\s'
enddef

# ----------------------------------------------------------------------------
# 文件信息
# ----------------------------------------------------------------------------

# 自动更新文件中的 "Last Modified:" 时间戳
def g:AutoUpdateLastUpdateInfo()
  const origin_pos = getpos('.')
  const pattern = ' Last Modified:  '
  const match = searchpos(pattern)

  if match[0] == 0
    return
  endif

  const line_text = getline(match[0])
  const prefix_len = match[1] + strlen(pattern)
  const updated_line = line_text[: prefix_len - 2] .. strftime('%Y-%m-%d %H:%M:%S')
  setline(match[0], updated_line)
  setpos('.', origin_pos)
enddef

# ----------------------------------------------------------------------------
# 字符串
# ----------------------------------------------------------------------------

# 判断字符串是否为空
def g:IsEmpty(str: any): bool
  return empty(str)
enddef

# 安全获取字典值
def g:SafeGet(dict: dict<any>, key: string, default: any = ''): any
  return get(dict, key, default)
enddef

# ----------------------------------------------------------------------------
# 路径
# ----------------------------------------------------------------------------

# 规范化路径（处理 Windows/Linux 差异）
def g:NormalizePath(path: string): string
  const unix_path = substitute(path, '\\', '/', 'g')
  const separator = CurrentPathSeparator()
  return separator ==# '/' ? unix_path : tr(unix_path, '/', separator)
enddef

# 检查路径是否在配置目录下
def g:IsInConfigDir(path: string): bool
  const config_root = NormalizeComparablePath(g:mcge_customvimrcdir)
  const target_path = NormalizeComparablePath(path)
  return IsPathWithin(config_root, target_path)
enddef

# ----------------------------------------------------------------------------
# 文件类型
# ----------------------------------------------------------------------------

# 获取当前 buffer 的文件类型
def g:GetCurrentFiletype(): string
  return &filetype
enddef

# 检查是否为指定的文件类型
def g:IsFiletype(ft: string): bool
  return &filetype ==# ft
enddef

# vim: set ft=vim sw=2 ts=2 sts=2 et:
