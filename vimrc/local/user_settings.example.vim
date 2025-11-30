vim9script
# ============================================================================
# 用户自定义配置
# 作者: mcge <mcgeq@outlook.com>
# 说明: 此文件用于覆盖默认配置，不会被 Git 跟踪
# ============================================================================

# ----------------------------------------------------------------------------
# 配置加载完成事件
# ----------------------------------------------------------------------------
autocmd User McgeConfigLoaded {
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
}

# ----------------------------------------------------------------------------
# 你的自定义设置
# ----------------------------------------------------------------------------

# 示例：覆盖默认设置
# set number
# set relativenumber

# 示例：自定义快捷键
# nnoremap <leader>h :echo "Hello from user config!"<CR>

# 示例：自定义颜色方案
# colorscheme desert

# vim: set ft=vim sw=2 ts=2 sts=2 et:
