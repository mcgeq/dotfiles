# Local Overrides

`local/` 用于存放用户或机器本地覆盖项。
这一层保留给个人偏好、路径差异和实验性调整，不承担共享模块职责。

## Ownership

- `user_env.vim`
  适合放用户身份、Shell、工作目录、外部工具路径等默认值。
  这里的配置应尽量使用“仅在未定义时设置”的方式，方便不同机器复用。

- `user_settings.vim`
  适合放最终覆盖项、启动完成后的自定义提示、个人 UI 偏好和本地 autocmd。
  建议按固定 hook 分区：
  `ApplyUserEditorOverrides()`、`ApplyUserUiOverrides()`、
  `RegisterUserCommands()`、`RegisterUserAutocmds()`。
  如果要注册长期存在的 autocmd，建议也像共享模块一样使用 augroup。

- `user_mappings.vim`
  可选文件。
  适合放只属于当前用户的快捷键，不应回写到共享的 `config/mapping/*.vim`。
  可以从 `user_mappings.example.vim` 复制开始。

- `module_overrides.vim`
  可选文件。
  适合集中覆盖 `modules/*` 的 `config` 字段，不必直接修改共享模块文件。
  可以从 `module_overrides.example.vim` 复制开始。

## Rules

- `local/` 中的内容以个人覆盖为主，不要重新承担共享模块或插件初始化职责。
- 共享逻辑放回 `modules/`、`config/` 或 `after/ftplugin/`，避免把公共配置漂移到本地层。
- 本地路径、账号、Shell 和工作区差异优先放在 `user_env.vim`。
- 需要统一覆盖某个共享模块的配置时，优先放在 `module_overrides.vim`。
- 需要覆盖默认选项或追加个人 autocmd 时，优先放在 `user_settings.vim`。
- `user_settings.vim` 内优先遵守固定 hook 分区：
  编辑器选项放 `ApplyUserEditorOverrides()`，
  UI 偏好放 `ApplyUserUiOverrides()`，
  本地命令放 `RegisterUserCommands()`，
  autocmd 放 `RegisterUserAutocmds()`。
- 如果本地 `autocmd` 需要长期存在，优先定义成可重复 source 的 augroup，
  避免每次重载时重复注册。
- 这些文件不会被 Git 跟踪；删除本地覆盖后，配置仍应能回退到共享默认值。

## Examples

```vim
" local/user_env.vim
call SetGlobalDefault('mcge_custom_workspace', 'D:/workspaces')
call SetGlobalDefault('mcge_custom_shell', 'pwsh')
```

```vim
" local/user_settings.vim
def ApplyUserEditorOverrides()
  set number
enddef

def ApplyUserUiOverrides()
  set cursorline
enddef

def RegisterUserAutocmds()
  augroup mcge_local_user_custom
    autocmd!
    autocmd BufEnter *.md setlocal wrap
  augroup END
enddef
```

```vim
" local/user_mappings.vim
nnoremap <silent> <leader>w <Cmd>write<CR>
nnoremap <silent> <leader>bd <Cmd>bdelete<CR>
```

```vim
" local/module_overrides.vim
g:SetModuleOverride('colorscheme', {scheme: 'desert'})
g:SetModuleOverride('clap', {layout: {width: '90%'}})
```

## Related

- `../ARCHITECTURE.md`
- `../README.md`
