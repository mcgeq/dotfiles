vim9script
# ============================================================================
# Clap 配置
# vim-clap: 现代化的模糊搜索插件
# 官方文档: https://github.com/liuchengxu/vim-clap
# ============================================================================

# ----------------------------------------------------------------------------
# 外观设置 - UI 优化
# ----------------------------------------------------------------------------
# 主题选择（推荐：material_design_dark 或 nord）
g:clap_theme = 'material_design_dark'
# 其他推荐主题:
# - 'atom_dark'          - Atom 风格暗色主题
# - 'gruvbox_dark'       - Gruvbox 暗色
# - 'nord'               - Nord 优雅蓝色主题
# - 'solarized_dark'     - Solarized 暗色
# - 'molokai'            - Molokai 经典

# 启用 Nerd Fonts 图标（更美观）
g:clap_enable_icon = 1

# 窗口布局 - 更大更居中
g:clap_layout = {
  'relative': 'editor',
  'width': '85%',
  'height': '50%',
  'row': '15%',
  'col': '7%'
}

# 启用背景阴影效果
g:clap_enable_background_shadow = 1

# 搜索框输入提示
g:clap_prompt_format = '%provider_id%>>'

# 启用搜索结果数量显示
g:clap_enable_indicator = 1

# ----------------------------------------------------------------------------
# 预览和性能
# ----------------------------------------------------------------------------
# 预览窗口配置
g:clap_preview_size = 10                   # 预览行数（更多上下文）
g:clap_preview_direction = 'UD'            # 预览方向：UD(上下) or LR(左右)

# 搜索延迟和动画
g:clap_provider_grep_delay = 200           # grep 搜索延迟（更快响应）
g:clap_provider_grep_blink = [2, 100]      # 高亮闪烁 2 次，更明显

# 缓存配置
g:clap_cache_directory = expand('~/.cache/clap')

# ----------------------------------------------------------------------------
# 搜索工具
# ----------------------------------------------------------------------------
# 使用 ripgrep 和 fd（性能最佳）
if executable('rg')
  g:clap_provider_grep_executable = 'rg'
  g:clap_provider_grep_opts = '--vimgrep --smart-case --hidden'
endif

if executable('fd')
  g:clap_provider_files_executable = 'fd'
  g:clap_provider_files_opts = '--type f --hidden --follow --exclude .git --exclude node_modules'
endif

# ----------------------------------------------------------------------------
# Provider 配置
# ----------------------------------------------------------------------------
g:clap_provider_history_size = 50          # 历史文件数量
g:clap_insert_mode_only = 0                 # 支持普通模式导航

# ----------------------------------------------------------------------------
# 高级选项
# ----------------------------------------------------------------------------
# 使用内置 Rust 实现（最快）
g:clap_force_python = 0

# 自定义高亮颜色（可选，配合主题使用）
# 在 colorscheme 加载后设置以下高亮
# highlight ClapInput guifg=#C0CAF5 guibg=#1f2335
# highlight ClapDisplay guibg=#1a1b26
# highlight ClapPreview guibg=#24283b
# highlight ClapMatches guifg=#ff9e64 gui=bold
# highlight ClapSelected guifg=#7dcfff guibg=#283457
