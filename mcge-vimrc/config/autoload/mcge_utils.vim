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

