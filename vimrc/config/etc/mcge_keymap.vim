vim9script

var mapping_dir = g:mcge_customvimrcdir .. '/config/mapping'

def SourceIfExists(fname: string)
    if filereadable(fname)
        execute $'source {fname}'
    endif
enddef

var mapping_files = [
    mapping_dir .. '/basic.vim',
    mapping_dir .. '/coc.vim',
    mapping_dir .. '/git.vim',
    mapping_dir .. '/navigation.vim',
    mapping_dir .. '/terminal.vim',
]

for f in mapping_files
    call SourceIfExists(f)
endfor
