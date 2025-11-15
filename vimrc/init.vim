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

var core_files = sort(mcge_utils.GetAllVimFilesInDirectory(g:mcge_customvimrcdir .. "/config/core"))

for v in core_files
    execute $"source {v}"
endfor

var etc_files = sort(mcge_utils.GetAllVimFilesInDirectory(g:mcge_customvimrcdir .. "/config/etc"))

for v in etc_files
    execute $"source {v}"
endfor

var plugin_files = sort(mcge_utils.GetAllVimFilesInDirectory(g:mcge_customvimrcdir .. "/config/plugins"))

for v in plugin_files
    execute $"source {v}"
endfor

var lang_files = sort(mcge_utils.GetAllVimFilesInDirectory(g:mcge_customvimrcdir .. "/config/lang"))

for v in lang_files
    execute $"source {v}"
endfor

# 保存文件时自动更新日期时间
autocmd BufWritePre *.{rs,c,cpp,py,ts,cs} mcge_utils.AutoUpdateLastUpdateInfo() 

