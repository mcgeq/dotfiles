vim9script

var s_plugin = g:mcge_customvimrcdir .. '/config/plugins/ui/airline.vim'

if filereadable(s_plugin)
    execute $'source {s_plugin}'
endif
