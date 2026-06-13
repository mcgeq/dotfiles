vim9script
# ============================================================================
# 组件: Local / UserSettings
# 作者: mcge <mcgeq@outlook.com>
# 说明: 用户本地覆盖与启动完成后的自定义行为示例。
# ============================================================================

# ----------------------------------------------------------------------------
# 启动提示
# ----------------------------------------------------------------------------
def g:ShowUserStartupSummary()
  # 显示配置加载完成信息
  echo ''
  echohl Title
  echo '╔════════════════════════════════════════╗'
  echo '║   Vim 配置已加载 (v2.0 优化版) ✓      ║'
  echo '╚════════════════════════════════════════╝'
  echohl None
  
  # 显示性能信息
  if exists('g:mcge_startup_time')
    const time = g:mcge_startup_time
    echohl String
    echo $'  ⚡ 启动时间: {printf("%.2f", time)}ms'
    echohl None
    
    # 性能评价
    if time < 100
      echohl String
      echo '  🚀 启动速度: 极快'
      echohl None
    elseif time < 200
      echohl None
      echo '  ✓ 启动速度: 良好'
    else
      echohl WarningMsg
      echo '  ⚠ 启动速度: 可优化'
      echohl None
    endif
  endif
  
  # 显示模块加载统计
  if exists('*g:GetLoadStats')
    const stats = g:GetLoadStats()
    echo $'  📦 已加载模块: {stats.loaded}/{stats.total}'
    
    if stats.failed > 0
      echohl ErrorMsg
      echo $'  ✗ 失败模块: {stats.failed}'
      echohl None
    endif
  endif
  
  # 显示可用命令提示
  echo ''
  echohl Comment
  echo '  提示: 使用 <Space> 键查看所有快捷键'
  echo '        使用 :VimStartupTime 查看启动时间'
  echo '        使用 :VimrcLoadReport 查看加载报告'
  echohl None
  echo ''
enddef

def RegisterUserStartupAutocmds()
  augroup mcge_local_user_settings
    autocmd!
    autocmd User McgeConfigLoaded call g:ShowUserStartupSummary()
  augroup END
enddef

RegisterUserStartupAutocmds()

# ----------------------------------------------------------------------------
# 用户覆盖约定
# ----------------------------------------------------------------------------

def ApplyUserEditorOverrides()
  # 放编辑器选项、set、本地默认行为。
  set number
  set relativenumber
enddef

def ApplyUserUiOverrides()
  # 放个人 UI / 主题 / 展示偏好。
  set cursorline
  # colorscheme desert
enddef

def RegisterUserCommands()
  # 放仅属于当前用户的本地命令。
  command! UserHello echo 'Hello from user config!'
enddef

def RegisterUserAutocmds()
  # 放个人自动命令。请始终使用独立 augroup，保证可重复 source。
  augroup mcge_local_user_custom
    autocmd!
    autocmd BufEnter *.md setlocal wrap
  augroup END
enddef

def g:InitUserSettings()
  ApplyUserEditorOverrides()
  ApplyUserUiOverrides()
  RegisterUserCommands()
  RegisterUserAutocmds()
enddef

g:InitUserSettings()

# vim: set ft=vim sw=2 ts=2 sts=2 et:
