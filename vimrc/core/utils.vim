vim9script
# ============================================================================
# 工具函数库
# 作者: mcge <mcgeq@outlook.com>
# 描述: 从 mcge_utils.vim 迁移并扩展
# 注意: 本文件通过 source 加载，使用全局函数 g:
# ============================================================================

# ----------------------------------------------------------------------------
# 文件和目录操作
# ----------------------------------------------------------------------------

# 获取指定目录下的所有 .vim 文件（递归）
def g:GetAllVimFiles(directory: string): list<string>
    var files: list<string> = []
    GetVimFilesRecursively(directory, files)
    return files
enddef

def GetVimFilesRecursively(directory: string, files: list<string>)
    if !isdirectory(directory)
        return
    endif
    
    const directoryFiles: list<string> = glob(directory .. '/*', 0, 1)
    
    for item in directoryFiles
        if isdirectory(item)
            GetVimFilesRecursively(item, files)
        else
            if item =~# '\.vim$'
                add(files, item)
            endif
        endif
    endfor
enddef

# ----------------------------------------------------------------------------
# CoC 相关工具函数
# ----------------------------------------------------------------------------

# 检查光标前是否为空白
def g:CheckBackspace(): bool
  var col = col('.') - 1
  return !col || getline('.')[col - 1]  =~# '\s'
enddef

# ----------------------------------------------------------------------------
# 文件信息更新
# ----------------------------------------------------------------------------

# 自动更新文件中的 "Last Modified:" 时间戳
def g:AutoUpdateLastUpdateInfo()
    var origin_pos = getpos('.')
    var regexp = " Last Modified:  "
    var lu = searchpos(regexp)
    
    if lu[0] != 0
        var oln = getline(lu[0])
        var str_len = lu[1] + strlen(regexp)
        var nline = oln[ : str_len - 2]
        call setline(lu[0], nline .. strftime('%Y-%m-%d %H:%M:%S'))
        call setpos(".", origin_pos)
    endif
enddef

# ----------------------------------------------------------------------------
# 字符串工具
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
# 路径工具
# ----------------------------------------------------------------------------

# 规范化路径（处理 Windows/Linux 差异）
def g:NormalizePath(path: string): string
    if has('win32') || has('win64')
        return substitute(path, '/', '\', 'g')
    else
        return substitute(path, '\', '/', 'g')
    endif
enddef

# 检查路径是否在配置目录下
def g:IsInConfigDir(path: string): bool
    return path =~# '^' .. g:mcge_customvimrcdir
enddef

# ----------------------------------------------------------------------------
# 文件类型工具
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
