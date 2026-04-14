vim9script
# ============================================================================
# 组件: Local / UserEnv
# 作者: mcge <mcgeq@outlook.com>
# 说明: 用户与机器本地默认值；仅在变量未定义时提供回退值。
# ============================================================================

# ----------------------------------------------------------------------------
# 内部辅助
# ----------------------------------------------------------------------------
def SetGlobalDefault(name: string, value: any)
  if !has_key(g:, name)
    g:[name] = value
  endif
enddef

# ----------------------------------------------------------------------------
# 用户身份
# ----------------------------------------------------------------------------
call SetGlobalDefault('mcge_custom_author', 'mcge')
call SetGlobalDefault('mcge_custom_email', '<mcgeq@outlook.com>')

# ----------------------------------------------------------------------------
# 工具默认值
# ----------------------------------------------------------------------------
# Vista 执行器（ctags 或 coc）
call SetGlobalDefault('mcge_custom_vista_executive', 'coc')

# Shell 类型（pwsh, powershell, bash, zsh 等）
call SetGlobalDefault('mcge_custom_shell', 'pwsh')

# ----------------------------------------------------------------------------
# 路径默认值
# ----------------------------------------------------------------------------
if has('unix')
  # Linux/Mac 路径
  call SetGlobalDefault('mcge_customvimrcdir', '~/dotfiles/vimrc')
  call SetGlobalDefault('mcge_custom_project', '~/projects')
  call SetGlobalDefault('mcge_custom_workspace', '~/workspace')
  call SetGlobalDefault('mcge_custom_preview_bash', '/usr/bin/bash')
else
  # Windows 路径（根据你的实际路径修改）
  call SetGlobalDefault('mcge_customvimrcdir', 'D:/config/dotfiles/vimrc')
  call SetGlobalDefault('mcge_custom_project', 'F:/2024/projects')
  call SetGlobalDefault('mcge_custom_workspace', 'D:/workspaces')
  call SetGlobalDefault('mcge_custom_preview_bash', 'D:/bin/Git/bin/bash.exe')
endif

# vim: set ft=vim sw=2 ts=2 sts=2 et:
