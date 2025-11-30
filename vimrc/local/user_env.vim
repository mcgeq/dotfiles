vim9script
# ============================================================================
# 用户环境变量配置
# 作者: mcge <mcgeq@outlook.com>
# 说明: 从 mcge-env.vim 迁移的用户自定义变量
# ============================================================================

# 辅助函数：只在变量未定义时设置
def SetIfUndef(name: string, value: any)
  if !exists(name)
    execute $"let {name} = value"
  endif
enddef

# ----------------------------------------------------------------------------
# 作者信息
# ----------------------------------------------------------------------------
call SetIfUndef("g:mcge_custom_author", "mcge")
call SetIfUndef("g:mcge_custom_email", "<mcgeq@outlook.com>")

# ----------------------------------------------------------------------------
# 工具配置
# ----------------------------------------------------------------------------
# Vista 执行器（ctags 或 coc）
call SetIfUndef("g:mcge_custom_vista_executive", "coc")

# Shell 类型（pwsh, powershell, bash, zsh 等）
call SetIfUndef("g:mcge_custom_shell", "pwsh")

# ----------------------------------------------------------------------------
# 路径配置（根据操作系统）
# ----------------------------------------------------------------------------
if has('unix')
  # Linux/Mac 路径
  call SetIfUndef("g:mcge_customvimrcdir", "~/dotfiles/vimrc")
  call SetIfUndef("g:mcge_custom_project", "~/projects")
  call SetIfUndef("g:mcge_custom_workspace", "~/workspace")
  call SetIfUndef("g:mcge_custom_fzf_dir", "~/.fzf")
  call SetIfUndef("g:mcge_custom_preview_bash", "/usr/bin/bash")
else
  # Windows 路径（根据你的实际路径修改）
  call SetIfUndef("g:mcge_customvimrcdir", "D:/config/dotfiles/vimrc")
  call SetIfUndef("g:mcge_custom_project", "F:/2024/projects")
  call SetIfUndef("g:mcge_custom_workspace", "D:/workspaces")
  call SetIfUndef("g:mcge_custom_fzf_dir", "D:/bin/fzf")
  call SetIfUndef("g:mcge_custom_preview_bash", "D:/bin/Git/bin/bash.exe")
endif

# vim: set ft=vim sw=2 ts=2 sts=2 et:
