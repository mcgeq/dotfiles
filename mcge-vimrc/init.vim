vim9script

var mcge_color = $"{g:mcge_customvimrcdir}/colors"
execute $"set runtimepath+={mcge_color}"

# ftplugin
var mcge_ftplugin = $"{g:mcge_customvimrcdir}/ftplugin"
execute $"set runtimepath+={mcge_ftplugin}"

# autoload
var mcge_autoload = $"{g:mcge_customvimrcdir}/config"
execute $"set runtimepath+={mcge_autoload}"

import autoload "mcge_utils.vim"

var core_files =  mcge_utils.GetAllVimFilesInDirectory(g:mcge_customvimrcdir .. "/config/core")

for v in core_files
    execute $"source {v}"
endfor

var etc_files =  mcge_utils.GetAllVimFilesInDirectory(g:mcge_customvimrcdir .. "/config/etc")

for v in etc_files
    execute $"source {v}"
endfor
