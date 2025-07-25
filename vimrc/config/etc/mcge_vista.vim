vim9script

# 设置默认的执行
g:vista_default_executive = $"{g:mcge_custom_vista_executive}"
# 设置vista窗口的宽度
g:vista_sidebar_width = 30
# 设置vista 窗口图标的缩进
g:vista_icon_indent = 3
# 高亮整行
g:vista_highlight_whole_line = 1
# 禁用Vista 状态栏
# g:vista_disable_statueline = 1
# 禁用默认的vista映射, 自定义映射 见 mcge_keymap.vim 
# g:vista_no_mappings = 1
g:vista_fzf_preview = ["right:50%"]
# 焦点自动跳转到图标
g:vista_focus_on_icons = 1
# 设置光标移动时 vista 窗口如何显示
g:vista_echo_cursor_strategy = "floating_win"
# 在 vista 窗口中启用浮动预览
g:vista_floating_preview = 1

g:vista#renderer#enable_icon = 1

g:vista#renderer#icons = {
    "function": "\uf794",
    "variable": "\uf71b", }
