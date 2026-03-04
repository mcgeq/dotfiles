vim9script
# ============================================================================
# vim-matchup 配置 - 增强的匹配导航
# ============================================================================

let g:matchup_enabled = 1
let g:matchup_matchparen_offscreen = {'method': 'popup'}
let g:matchup_matchparen_highlight = 1
let g:matchup_delim_noskips = 2
let g:matchup_mappings_enabled = 1
let g:matchup_text_obj_enabled = 1

omap i <Plug>(matchup-i%)
omap a <Plug>(matchup-a%)
xmap i <Plug>(matchup-i%)
xmap a <Plug>(matchup-a%)
nmap g% <Plug>(matchup-g%)
nmap [% <Plug>(matchup-[%)
nmap ]% <Plug>(matchup-]%)
nmap z% <Plug>(matchup-z%)

# vim: set ft=vim sw=2 ts=2 sts=2 et:
