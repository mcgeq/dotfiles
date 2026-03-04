vim9script
# ============================================================================
# vim-gitgutter 配置 - Git状态显示
# ============================================================================

g:gitgutter_enabled = 1
g:gitgutter_async = 1

g:gitgutter_sign_added = '│'
g:gitgutter_sign_modified = '┌'
g:gitgutter_sign_removed = '╵'
g:gitgutter_sign_removed_first_line = '█'
g:gitgutter_sign_modified_removed = '╜'

g:gitgutter_summary = 1
g:gitgutter_override_highlight = 0
g:gitgutter_sign_column_always = 1
g:gitgutter_preview_win_floating = 1

nmap ]c <Plug>(GitGutterNextHunk)
nmap [c <Plug>(GitGutterPrevHunk)
nmap <leader>hs <Plug>(GitGutterStageHunk)
nmap <leader>hu <Plug>(GitGutterUndoHunk)
nmap <leader>hp <Plug>(GitGutterPreviewHunk)

# vim: set ft=vim sw=2 ts=2 sts=2 et:
