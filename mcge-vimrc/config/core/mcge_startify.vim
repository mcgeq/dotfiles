vim9script

# 自定义Startify的Header和Footer
g:startify_custom_header = [
"mm mm mmmm    cccc  gggggg   eee",
"mmm  mm  mm cc     gg   gg ee   e", 
"mmm  mm  mm cc     ggggggg eeeee",
"mmm  mm  mm  ccccc      gg  eeeee",
"                    ggggg",
\]

# 起始页显示的列表长度
g:startify_files_number = 10
# 自动加载session
g:startify_session_autoload = 1
# 设置缩略图
g:startify_fortune_use_unicode = 1

# 设置 Startify 显示的文件列表
g:startify_lists = [
    { 'type': 'bookmarks', 'header': ['   Bookmarks'] },
    { 'type': 'files', 'header': ['   Recent Files'] },
    { 'type': 'dir', 'header': ['   Project Directory'] },
    ]

# 设置 Startify 书签
g:startify_bookmarks = [
    { 'c': $"{g:mcge_customvimrcdir}/init.vim" },
    { 'p': $"{g:mcge_custom_project}"},
    { 'w': $"{g:mcge_custom_workspace}" },
    ]
# 过滤列表
g:startify_skiplist = [
    '^/tmp',
]