vim9script

# 统一管理 mcge 系列全局变量的默认值
# 注意：只在变量尚未由主 vimrc 设置时才赋默认值，避免覆盖你的自定义。

# 小工具：如果变量未定义，则设置默认值
def SetIfUndef(name: string, value: any)
  if !exists(name)
    execute $"let {name} = value"
  endif
enddef

# 作者 / 版权信息
call SetIfUndef("g:mcge_custom_author", "mcge")
call SetIfUndef("g:mcge_custom_email", "<mcgeq@outlook.com>")
call SetIfUndef("g:mcge_custom_vista_executive", "ctags")
call SetIfUndef("g:mcge_custom_shell", "pwsh")

# 默认路径：仅在主 vimrc 未设置时兜底
if has('unix')
  call SetIfUndef("g:mcge_customvimrcdir", "~/dotfiles/vimrc")
  call SetIfUndef("g:mcge_custom_project", "~/projects")
  call SetIfUndef("g:mcge_custom_workspace", "~/workspace")
  call SetIfUndef("g:mcge_custom_fzf_dir", "~/.fzf")
  call SetIfUndef("g:mcge_custom_preview_bash", "/usr/bin/bash")
else
  call SetIfUndef("g:mcge_customvimrcdir", "D:/config/dotfiles/vimrc")
  call SetIfUndef("g:mcge_custom_project", "F:/2024/projects")
  call SetIfUndef("g:mcge_custom_workspace", "D:/workspaces")
  call SetIfUndef("g:mcge_custom_fzf_dir", "D:/bin/fzf")
  call SetIfUndef("g:mcge_custom_preview_bash", "D:/bin/Git/bin/bash.exe")
endif
