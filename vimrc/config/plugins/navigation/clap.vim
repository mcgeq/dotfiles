vim9script
# ============================================================================
# Clap 配置
# vim-clap: 现代化的模糊搜索插件
# ============================================================================

# 主题设置
g:clap_theme = 'material_design_dark'
# 可选主题: 'atom_dark', 'gruvbox_dark', 'nord', 'solarized_dark', 'molokai'

# 启用图标
g:clap_enable_icon = 1

# 搜索框布局
g:clap_layout = {
  'relative': 'editor',
  'width': '80%',
  'height': '40%',
  'row': 0.1,
  'col': 0.1
}

# 性能优化
g:clap_preview_size = 5              # 预览行数
g:clap_provider_grep_delay = 300     # grep 延迟（ms）
g:clap_preview_direction = 'UD'      # 预览方向：UD(上下) or LR(左右)

# 启用背景作业（异步搜索）
g:clap_enable_background_shadow = 1

# 默认使用的搜索工具
# g:clap_provider_grep_executable = 'rg'  # 或 'ag', 'grep'
