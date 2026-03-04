vim9script
# ============================================================================
# vim-better-whitespace 配置 - 空白字符高亮
# ============================================================================

let g:better_whitespace_enabled = 1
let g:strip_whitespace_on_save = 1
let g:better_whitespace_filetypes_blacklist = ['diff', 'gitcommit', 'markdown', 'xml']
let g:better_whitespace_color = 'CursorColumn'
let g:current_line_whitespace_disabled_actively = 1

nmap <leader>tw :ToggleWhitespace<CR>
nmap <leader>ts :StripWhitespace<CR>

# vim: set ft=vim sw=2 ts=2 sts=2 et:
