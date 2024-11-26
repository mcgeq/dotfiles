vim9script

# 获取指定目录下的所有以.vim结尾的文件
export def g:GetAllVimFilesInDirectory(directory: string): list<string>
    # 初始化一个空列表存储结果
    var files: list<string> = []
    # 获取指定路径下所有以.vim结尾的文件
    GetVimFilesRecursively(directory, files)
    return files
enddef

def GetVimFilesRecursively(directory: string, files: list<string>)
    # 获取当前目录下所有文件和目录
    var directoryFiles: list<string> = glob(directory .. '/*', 0, 1)
    # 遍历当前目录下所有文件和目录
    for item in directoryFiles
        # 目录则递归调用自身
        if isdirectory(item)
            GetVimFilesRecursively(item, files)
        else
            # .vim结尾的文件
            if item =~# '\.vim$'
                add(files, item)
            endif
        endif
    endfor
enddef


export def CheckBackspace(): bool
  var col = col('.') - 1
  return !col || getline('.')[col - 1]  =~# '\s'
enddef

export def ShowDocumentation()
  if CocAction('hasProvider', 'hover')
    call CocActionAsync('doHover')
  else
    call feedkeys('K', 'in')
  endif
enddef

export def g:AutoUpdateLastUpdateInfo()
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
    # var cur_time = strftime('%Y-%m-%d %H:%M:%S')
    # execute ':s/\(Last Modified:\s\+\)\d\{4}-\d\{2}-\d\{2} \d\{2}:\d\{2}:\d\{2}\(.*\)/\1' .. cur_time .. '\2/'
enddef
