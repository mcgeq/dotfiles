vim9script
# ============================================================================
# 模块: UI / WhichKey
# 作者: mcge <mcgeq@outlook.com>
# 说明: 配置 WhichKey 弹窗和快捷键注册行为。
# ============================================================================

# 防止重复加载
if g:MarkModuleLoaded('whichkey')
  finish
endif

var config = {
  enabled: true,
  trigger: '<Space>',
  visual_trigger: '<Space>',
  popup_laststatus: 0,
  restore_laststatus: 2,
}

def g:RefreshWhichKeyMaps()
  if g:ModuleIsDisabled(config, 'WhichKey')
    return
  endif

  g:which_key_map = ReadWhichKeyMap('n')
  g:which_key_map_visual = ReadWhichKeyMap('v')

  if !g:FunctionExists('which_key#register')
    return
  endif

  call('which_key#register', [config.trigger, 'g:which_key_map', 'n'])
  call('which_key#register', [config.visual_trigger, 'g:which_key_map_visual', 'v'])
enddef

def g:ApplyWhichKeyWindowOptions()
  &laststatus = config.popup_laststatus
  set noshowmode
  set noruler
enddef

def g:MaybeRestoreWhichKeyWindowOptions(bufref: any)
  const bufnr = type(bufref) == v:t_number ? bufref : str2nr(string(bufref))
  if bufnr <= 0 || getbufvar(bufnr, '&filetype') !=# 'which_key'
    return
  endif

  &laststatus = config.restore_laststatus
  set showmode
  set ruler
enddef

def ReadWhichKeyMap(mode: string): dict<any>
  if exists('*g:GetWhichKeyMap')
    const prefix = mode ==# 'v' ? config.visual_trigger : config.trigger
    return g:GetWhichKeyMap(prefix, mode)
  endif

  return {}
enddef

def RegisterWhichKeyAutocmds()
  augroup mcge_whichkey
    autocmd!
    autocmd User McgeConfigLoaded call g:RefreshWhichKeyMaps()
    autocmd FileType which_key call g:ApplyWhichKeyWindowOptions()
    autocmd BufLeave * call g:MaybeRestoreWhichKeyWindowOptions(expand('<abuf>'))
  augroup END
enddef

def ApplyWhichKeyHighlights()
  hi! link WhichKeyFloating Normal
  hi! link WhichKey Function
  hi! link WhichKeySeperator DiffAdded
  hi! link WhichKeyGroup Keyword
  hi! link WhichKeyDesc Identifier
enddef

def g:InitWhichKey(user_config: dict<any> = {})
  config = g:ResolveModuleConfig('whichkey', config, user_config)

  if g:ModuleIsDisabled(config, 'WhichKey')
    return
  endif

  g:RefreshWhichKeyMaps()
  RegisterWhichKeyAutocmds()
  ApplyWhichKeyHighlights()
enddef

def g:WhichKeyHealthCheck(): dict<any>
  return g:BuildManagedFunctionModuleHealth('whichkey', 'WhichKey', config, 'which_key#register', {
    trigger: config.trigger,
    visual_trigger: config.visual_trigger,
    registry_available: exists('*g:GetWhichKeyMap'),
  })
enddef

def g:GetWhichKeyConfig(): dict<any>
  return config
enddef

call g:InitWhichKey()
