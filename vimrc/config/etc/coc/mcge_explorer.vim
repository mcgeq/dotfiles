vim9script

# 启用 vim-devicons 插件
g:WebDevIconsUnicodeDecorateFolderNodes = 1
g:WebDevIconsUnicodeDecorateFileNodes = 1
g:WebDevIconsUnicodeGlyphDoubleWidthDirArrow = 0

# Presets

g:coc_explorer_global_presets = {
    ".vim": {
        "root-uri": $"{g:mcge_customvimrcdir}",
\    },
    "cocConfig": {
        "root-uri": $"{g:mcge_customvimrcdir}",
\    },
    "tab": {
        "position": "tab",
        "quit-on-open": v:true,
\    },
    "tab:$": {
        "position": "tab:$",
        "quit-on-open": v:true,
\    },
    "floating": {
        "position": "floating",
        "open-action-strategy": "sourceWindow",
\    },
    "floatingTop": {
        "position": "floating",
        "floating-position": "center-top",
        "open-on-strategy": "sourceWindow",
\    },
    "floatingLeftside": {
        "position": "floating",
        "floating-position": "left-center",
        "floating-width": 50,
        "open-on-strategy": "sourceWindow",
\    },
    "floatingRightside": {
        "position": "floating",
        "floating-position": "right-center",
        "floating-width": 50,
        "open-on-strategy": "sourceWindow",
\    },
    "simplify": {
        "file-child-template": "[selection | clip | 1] [indent][icon | 1] [filename omitCenter 1]"
\    },
    "buffer": {
        "sources": [{"name": "buffer", "expand": v:true}]
\    },
\ }
