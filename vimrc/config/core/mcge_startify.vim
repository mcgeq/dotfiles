vim9script

# -----------------------------------------------------------------------------
# Header / Footer
# -----------------------------------------------------------------------------
g:startify_custom_header = startify#center([
" _      ____  _____ _     _  _     ",
"/ \\__/|/   _\\/  __// \\ |\\/ \\/ \\__/|",
"| |\\/|||  /  | |  _| | //| || |\\/||",
"| |  |||  \\_ | |_//| \\\\// | || |  ||",
"\\_/  \\|\\____/\\____\\\\__/  \\_/\\_/  \\|",
"                                     ",
])

g:startify_custom_footer = startify#center([
    "",
    '  ⚡ Vim config by mcgeq',
])

# -----------------------------------------------------------------------------
# Lists & commands
# -----------------------------------------------------------------------------
g:startify_files_number = 10
g:startify_session_autoload = 1
g:startify_fortune_use_unicode = 1

g:startify_lists = [
    { 'type': 'commands', 'header': ['   Quick Actions'] },
    { 'type': 'files',     'header': ['   Recent Files'] },
    { 'type': 'bookmarks', 'header': ['   Bookmarks'] },
    { 'type': 'dir',       'header': ['   Project Directory'] },
]

g:startify_commands = [
    { 'n': ['  New File', 'enew'] },
    { 'f': ['  Find File', 'Files'] },
    { 'o': ['  Recents', 'History'] },
    { 'w': ['  Find Word', 'Rg'] },
    { 's': ['  Last Session', 'SLoad'] },
    { 'c': ['  Config', $'edit {g:mcge_customvimrcdir}/init.vim'] },
    { 'q': ['  Quit', 'quit'] },
]

# -----------------------------------------------------------------------------
# Bookmarks & filters
# -----------------------------------------------------------------------------
g:startify_bookmarks = [
    { 'i': $"{g:mcge_customvimrcdir}/init.vim" },
    { 'c': $"{g:mcge_customvimrcdir}/config" },
    { 'p': $"{g:mcge_custom_project}"},
    { 'w': $"{g:mcge_custom_workspace}" },
]

g:startify_skiplist = [
    '^/tmp',
]

# -----------------------------------------------------------------------------
# Layout helpers：根据窗口宽度动态设置左侧填充，实现近似居中
# -----------------------------------------------------------------------------
const startify_content_width = 48

def StartifyPadding(): number
    var padding = float2nr((&columns - startify_content_width) / 2)
    return max([0, padding])
enddef

def SetStartifyPadding()
    var pad = StartifyPadding()
    g:startify_padding_left = pad
    g:startify_padding_right = pad
enddef

call SetStartifyPadding()

augroup mcge_startify_layout
    autocmd!
    autocmd VimEnter *  call SetStartifyPadding()
    autocmd VimResized * call SetStartifyPadding()
augroup END