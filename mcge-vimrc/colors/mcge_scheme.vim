vim9script

# 定义函数 MyCustomColors
def MyCustomColors()
    # 设置正常文本颜色为白色，背景色为黑色
    highlight Normal guifg=#ffffff guibg=#000000 ctermfg=white ctermbg=black
    # 设置注释颜色为灰色
    highlight Comment guifg=#808080 ctermfg=darkgrey
    # 设置函数名颜色为蓝色，粗体
    highlight Function guifg=#0000ff ctermfg=blue gui=bold cterm=bold
    # 设置字符串颜色为绿色
    highlight String guifg=#00ff00 ctermfg=green
    # 设置关键字颜色为橙色
    highlight Keyword guifg=#ff7f00 ctermfg=darkyellow
    # 设置光标下字符的背景色为淡蓝色
    highlight CursorLine guibg=#add8e6 ctermbg=lightblue
enddef

MyCustomColors()