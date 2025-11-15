vim9script

# This is the default extra key bindings
g:fzf_action = {
  \ 'ctrl-t': 'tab split',
  \ 'ctrl-x': 'split',
  \ 'ctrl-v': 'vsplit' }

# An action can be a reference to a function that processes selected lines
# function! s:build_quickfix_list(lines)
#  call setqflist(map(copy(a:lines), '{ "filename": v:val, "lnum": 1 }'))
#  copen
#  cc
# endfunction
#  \ 'ctrl-q': function('s:build_quickfix_list'),

g:fzf_action = {
  \ 'ctrl-t': 'tab split',
  \ 'ctrl-x': 'split',
  \ 'ctrl-v': 'vsplit' }

# Default fzf layout
# - Popup window (center of the screen)
g:fzf_layout = { 'window': { 'width': 0.9, 'height': 0.6, 'border': 'horizontal' } }

# - Popup window (center of the current window)
g:fzf_layout = { 'window': { 'width': 0.9, 'height': 0.6, 'border': 'horizontal', 'relative': v:true } }

# - Popup window (anchored to the bottom of the current window)
g:fzf_layout = { 'window': { 'width': 0.9, 'height': 0.6, 'border': 'horizontal', 'relative': v:true, 'yoffset': 1.0 } }

# - down / up / left / right
g:fzf_layout = { 'down': '40%' }

# - Window using a Vim command
g:fzf_layout = { 'window': 'enew' }
g:fzf_layout = { 'window': '-tabnew' }
g:fzf_layout = { 'window': '10new' }

# Customize fzf colors to match your color scheme
# - fzf#wrap translates this to a set of `--color` options
g:fzf_colors = { 
   'fg':      ['fg', 'Normal'],
   'bg':      ['bg', 'Normal'],
   'hl':      ['fg', 'Comment'],
   'fg+':     ['fg', 'CursorLine', 'CursorColumn', 'Normal'],
   'bg+':     ['bg', 'CursorLine', 'CursorColumn'],
   'hl+':     ['fg', 'Statement'],
 \ 'info':    ['fg', 'PreProc'],
 \ 'border':  ['fg', 'Ignore'],
 \ 'prompt':  ['fg', 'Conditional'],
 \ 'pointer': ['fg', 'Exception'],
 \ 'marker':  ['fg', 'Keyword'],
 \ 'spinner': ['fg', 'Label'],
 \ 'header':  ['fg', 'Comment'] }

# Enable per-command history
# - History files will be stored in the specified directory
# - When set, CTRL-N and CTRL-P will be bound to 'next-history' and
#   'previous-history' instead of 'down' and 'up'.
g:fzf_history_dir = '~/.local/share/fzf-history'

g:fzf_vim = {}

g:fzf_vim.preview_window = ['hidden,right,50%,<70(up,40%)', 'ctrl-/'] 
