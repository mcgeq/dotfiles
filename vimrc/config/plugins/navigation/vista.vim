vim9script
# ============================================================================
# Vista 配置
# Vista.vim: 代码大纲和符号查看器
# 官方文档: https://github.com/liuchengxu/vista.vim
# ============================================================================

# ----------------------------------------------------------------------------
# Executive 配置（后端引擎）
# ----------------------------------------------------------------------------
# 默认使用 CoC（LSP），备选 ctags
g:vista_default_executive = $"{g:mcge_custom_vista_executive}"

# Executive 优先级和备选方案
# 如果 CoC 不可用，自动降级到 ctags
g:vista_executive_for = {
  'vim': 'ctags',
  'python': 'coc',
  'javascript': 'coc',
  'typescript': 'coc',
  'rust': 'coc',
  'go': 'coc',
  'c': 'coc',
  'cpp': 'coc',
  'java': 'coc'
}

# ----------------------------------------------------------------------------
# 外观设置
# ----------------------------------------------------------------------------
# 侧边栏宽度
g:vista_sidebar_width = 35

# 图标缩进
g:vista_icon_indent = ["▸ ", ""]

# 高亮当前符号所在行
g:vista_highlight_whole_line = 1

# 启用图标渲染
g:vista#renderer#enable_icon = 1

# 光标移动到 Vista 窗口时自动高亮
g:vista_cursor_delay = 200

# ----------------------------------------------------------------------------
# 浮动窗口配置
# ----------------------------------------------------------------------------
# 光标悬停时的显示策略
g:vista_echo_cursor_strategy = 'floating_win'

# 启用浮动预览
g:vista_floating_preview = 1

# 浮动窗口边框样式
g:vista_floating_border = 'rounded'

# 焦点自动跳转到符号
g:vista_focus_on_icons = 1

# ----------------------------------------------------------------------------
# Ctags 配置
# ----------------------------------------------------------------------------
# Ctags 命令（使用 Universal Ctags）
if executable('ctags')
  g:vista_ctags_cmd = {
    'haskell': 'hasktags -x -o - -c',
  }
endif

# Ctags 选项
g:vista_ctags_options = '--fields=+niazS --extras=+q'

# 递归搜索目录
g:vista_ctags_project_opts = '--recurse'

# ----------------------------------------------------------------------------
# 搜索集成
# ----------------------------------------------------------------------------
# 使用 Clap 进行符号搜索（更快更美观）
if exists(':Clap')
  g:vista_finder_alternative_executives = ['coc', 'ctags']
  # 使用 Clap 作为 finder
  # 可在 vista_keys.vim 中配置快捷键
endif

# ----------------------------------------------------------------------------
# 自动更新配置
# ----------------------------------------------------------------------------
# 保存文件时自动更新
g:vista_update_on_text_changed = 0           # 0: 禁用实时更新（性能更好）
g:vista_update_on_text_changed_delay = 500   # 延迟更新（ms）

# 光标移动时更新 echo
g:vista_echo_cursor = 1

# 关闭 Vista 窗口时的行为
g:vista_close_on_jump = 0                    # 0: 保持打开, 1: 跳转后关闭

# ----------------------------------------------------------------------------
# 图标配置（Nerd Fonts）
# ----------------------------------------------------------------------------
g:vista#renderer#icons = {
  'function': "\uf794",
  'variable': "\uf71b",
  'prototype': "\uf794",
  'macro': "\uf8a3",
  'class': "\uf0e8",
  'method': "\uf6a6",
  'struct': "\ufb44",
  'interface': "\ufa52",
  'enum': "\uf779",
  'field': "\uf93d",
  'constant': "\uf8ff",
  'property': "\ufab6",
  'module': "\uf668",
  'namespace': "\uf475",
  'package': "\ue612",
  'type': "\uf7fd",
  'typedef': "\uf7fd",
  'parameter': "\uf4a1",
  'default': "\uf29c"
}

# ----------------------------------------------------------------------------
# LSP (CoC) 专属配置
# ----------------------------------------------------------------------------
# CoC 符号类型过滤（可选，默认显示全部）
# g:vista_ignore_kinds = ['Variable']

# CoC 请求超时（ms）
g:vista_coc_timeout = 5000

# ----------------------------------------------------------------------------
# 性能优化
# ----------------------------------------------------------------------------
# 禁用状态栏（减少开销）
# g:vista_disable_statusline = 1

# 禁用 Vista 内部按键映射（使用自定义映射）
# g:vista_no_mappings = 1

# 延迟加载 Vista（首次打开时初始化）
g:vista_stay_on_open = 0                      # 0: 打开后光标回到编辑窗口

# Vista 窗口位置
g:vista_sidebar_position = 'vertical topleft' # 或 'vertical botright'

# ----------------------------------------------------------------------------
# 调试选项
# ----------------------------------------------------------------------------
# g:vista_echo_cursor_strategy = 'echo'  # 使用 echo 代替浮动窗口（调试用）
# g:vista_log_file = expand('~/.cache/vista/vista.log')
