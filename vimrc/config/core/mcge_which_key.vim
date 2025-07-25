vim9script
g:mapleader = "\<Space>"
g:maplocalleader = ','

nnoremap <silent> <leader> :<c-u>WhichKey '<Space>'<CR>
vnoremap <silent> <leader> :<c-u>WhichKeyVisual '<Space>'<CR>

call which_key#register('<Space>', "g:which_key_map")
call which_key#register('<Space>', "g:which_key_map", 'n')
call which_key#register('<Space>', "g:which_key_map_visual", 'v')

# Define prefix dictionary
g:which_key_map =  {}

g:which_key_map.f = { 'name': '+file' }

nnoremap <silent> <leader>fs :update<CR>
g:which_key_map.f.s = 'save-file'

nnoremap <silent> <leader>fd :e $MYVIMRC<CR>
g:which_key_map.f.d = 'open-vimrc'

nnoremap <silent> <leader>oq  :copen<CR>
nnoremap <silent> <leader>ol  :lopen<CR>

g:which_key_map.m = {
    "name": "+clap",
    "p": [":Clap", "providers"],
    "b": [":Clap buffers", "buffers"],
    "bc": [":Clap commits", "git commits"],
    "bcc": [":Clap bcommits", "current buffer git commits"],
    "c": [":Clap command", "command"],
    "h": [":Clap history", "file history"],
    "f": [":Clap files ++finder=rg --files --ignore --hidden", "search file"],
    "q": [":Clap quickfix", "quickfix list"],
    "j": [":Clap jumps", "jumps"],
    "m": [":Clap marks", "marks"],
    "w": [":Clap windows", "windows"],
    "t": [":Clap tags", "tags"],
    "s": [":Clap colors", "colors"],
    "g": [":Clap grep2", "find word"], }

g:which_key_map.w = {
    'name': '+windows',
    "w": ['<C-W>w', 'other-window'],
    "d": ['<C-W>c', 'delete-window'],
    "-": ['<C-W>s', 'split-window-below'],
    "|": ['<C-W>v', 'split-window-right'],
    "2": ['<C-W>v', 'layout-double-columns'],
    "h": ['<C-W>h', 'window-left'],
    "j": ['<C-W>j', 'window-below'],
    "l": ['<C-W>l', 'window-right'],
    "k": ['<C-W>k', 'window-up'],
    "H": ['<C-W>5<', 'expand-window-left'],
    "J": [':resize +5', 'expand-window-below'],
    "L": ['<C-W>5>', 'expand-window-right'],
    "K": [':resize -5', 'expand-window-up'],
    "=": ['<C-W>=', 'balance-window'],
    "s": ['<C-W>s', 'split-window-below'],
    "v": ['<C-W>v', 'split-window-below'],
    "?": ['Windows', 'fzf-window'], }

g:which_key_map.o = {
    'name': '+open',
    "q": 'open-quickfix',
    "l": 'open-locationlist', }

g:which_key_map.b = {
    'name': '+buffer',
    "1": ['b1', 'buffer 1'],
    "2": ['b2', 'buffer 2'],
    "d": ['bd', 'delete-buffer'],
    "f": ['bfirst', 'first-buffer'],
    "h": ['Startify', 'home-buffer'],
    "l": ['blast', 'last-buffer'],
    "n": ['bnext', 'next-buffer'],
    "p": ['bprevious', 'previous-buffer'],
    "?": ['Buffers', 'fzf-buffer'], }

g:which_key_map.l = {
    'name': '+lsp',
    "f": ['spacevim#lang#util#Format()', 'formatting'],
    "r": ['spacevim#lang#util#FindReferences()', 'references'],
    "R": ['spacevim#lang#util#Rename()', 'rename'],
    "s": ['spacevim#lang#util#DocumentSymbol()', 'document-symbol'],
    "S": ['spacevim#lang#util#WorkspaceSymbol()', 'workspace-symbol'],
    "g": {
      'name': '+goto',
      "d": ['spacevim#lang#util#Definition()', 'definition'],
      "t": ['spacevim#lang#util#TypeDefinition()', 'type-definition'],
      "i": ['spacevim#lang#util#Implementation()', 'implementation'], }, }
