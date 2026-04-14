vim9script
# ============================================================================
# 组件: Core / Keymap
# 作者: mcge <mcgeq@outlook.com>
# 说明: 为 config/mapping 提供声明式 keymap 注册能力。
# ============================================================================

# ----------------------------------------------------------------------------
# 注册表与基础辅助
# ----------------------------------------------------------------------------

def MergeMapSpec(defaults: dict<any>, spec: dict<any>): dict<any>
  var merged = copy(defaults)
  for [key, value] in items(spec)
    merged[key] = value
  endfor
  return merged
enddef

def g:MapSpec(spec: dict<any>, defaults: dict<any> = {}): dict<any>
  return MergeMapSpec(defaults, spec)
enddef

g:mcge_whichkey_registry = {}
g:mcge_keymap_registry = []

def EnsureWhichKeyPrefixRegistry(prefix: string): dict<any>
  if !has_key(g:mcge_whichkey_registry, prefix)
    g:mcge_whichkey_registry[prefix] = {n: {}, v: {}}
  endif
  return g:mcge_whichkey_registry[prefix]
enddef

def EnsureKeymapRegistry()
  if type(get(g:, 'mcge_keymap_registry', [])) != v:t_list
    g:mcge_keymap_registry = []
  endif
enddef

def GetPrefixAliases(prefix: string): list<string>
  if index(['<leader>', '<Space>', '<space>'], prefix) >= 0
    return ['<leader>', '<Space>', '<space>']
  endif

  return [prefix]
enddef

def MatchPrefixAlias(lhs: string, prefix: string): string
  for alias in GetPrefixAliases(prefix)
    if lhs[: len(alias) - 1] ==# alias
      return alias
    endif
  endfor

  return ''
enddef

def SplitKeySequence(input: string): list<string>
  if match(input, '<.\+>') == -1
    return split(input, '\zs')
  endif

  var keys: list<string> = []
  var start_new = true

  for char in split(input, '\zs')
    if start_new
      add(keys, char)
    else
      keys[-1] ..= char
    endif

    if char ==# '<'
      start_new = false
    elseif char ==# '>'
      start_new = true
    endif
  endfor

  return keys
enddef

def NormalizeWhichKeyModes(modes: any): list<string>
  var normalized: list<string> = []

  for mode in NormalizeModes(modes)
    if index(['', 'n'], mode) >= 0 && index(normalized, 'n') < 0
      add(normalized, 'n')
      continue
    endif

    if index(['v', 'x', 's'], mode) >= 0 && index(normalized, 'v') < 0
      add(normalized, 'v')
    endif
  endfor

  return normalized
enddef

def EnsureWhichKeyNode(root: dict<any>, keys: list<string>): dict<any>
  var current = root

  for key in keys
    if !has_key(current, key) || type(current[key]) != v:t_dict
      current[key] = {}
    endif
    current = current[key]
  endfor

  return current
enddef

def RegisterWhichKeyDescription(prefix: string, lhs: string, desc: string, modes: any)
  if empty(desc)
    return
  endif

  const matched_prefix = MatchPrefixAlias(lhs, prefix)
  if empty(matched_prefix)
    return
  endif

  const tail = strpart(lhs, len(matched_prefix))
  if empty(tail)
    return
  endif

  const keys = SplitKeySequence(tail)
  const prefix_registry = EnsureWhichKeyPrefixRegistry(prefix)

  for mode in NormalizeWhichKeyModes(modes)
    if empty(keys)
      continue
    endif

    if len(keys) == 1
      if has_key(prefix_registry[mode], keys[0]) && type(prefix_registry[mode][keys[0]]) == v:t_dict
        continue
      endif
      prefix_registry[mode][keys[0]] = desc
      continue
    endif

    var parent = EnsureWhichKeyNode(prefix_registry[mode], keys[: -2])
    if has_key(parent, keys[-1]) && type(parent[keys[-1]]) == v:t_dict
      continue
    endif
    parent[keys[-1]] = desc
  endfor
enddef

def RegisterWhichKeyGroup(prefix: string, path: string, name: string, modes: any)
  if empty(path) || empty(name)
    return
  endif

  const keys = SplitKeySequence(path)
  const prefix_registry = EnsureWhichKeyPrefixRegistry(prefix)

  for mode in NormalizeWhichKeyModes(modes)
    var group = EnsureWhichKeyNode(prefix_registry[mode], keys)
    group.name = name
  endfor
enddef

def g:WhichKeyGroup(spec: dict<any>)
  RegisterWhichKeyGroup(
    get(spec, 'prefix', '<Space>'),
    spec.path,
    spec.name,
    get(spec, 'modes', ['n', 'v']),
  )
enddef

def g:WhichKeyGroupMany(specs: list<dict<any>>, defaults: dict<any> = {})
  for spec in specs
    g:WhichKeyGroup(MergeMapSpec(defaults, spec))
  endfor
enddef

def g:GetWhichKeyMap(prefix: string, mode: string): dict<any>
  const prefix_registry = EnsureWhichKeyPrefixRegistry(prefix)
  return deepcopy(prefix_registry[mode])
enddef

def g:GetKeymapRegistry(): list<dict<any>>
  EnsureKeymapRegistry()
  return deepcopy(g:mcge_keymap_registry)
enddef

def NormalizeModes(modes: any): list<string>
  if type(modes) == v:t_string
    return modes ==# '' ? [''] : split(modes, '\zs')
  endif

  if type(modes) == v:t_list
    var normalized: list<string> = []
    for mode in modes
      add(normalized, mode)
    endfor
    return normalized
  endif

  throw $'Unsupported modes type: {type(modes)}'
enddef

def GetSpecModes(spec: dict<any>): list<string>
  if has_key(spec, 'modes')
    return NormalizeModes(spec.modes)
  endif

  if has_key(spec, 'mode')
    return NormalizeModes(spec.mode)
  endif

  throw $'Mapping spec missing "mode" or "modes": {string(spec)}'
enddef

def GetSpecRhs(spec: dict<any>, mode: string): string
  return GetRequiredSpecValue(spec, 'rhs', 'rhs_by_mode', mode)
enddef

def GetSpecDocRhs(spec: dict<any>, mode: string): string
  return GetSpecValueOrDefaultForMode(spec, 'doc_rhs', 'doc_rhs_by_mode', mode, GetSpecRhs(spec, mode))
enddef

def GetSpecDocAction(spec: dict<any>, mode: string): string
  return GetSpecValueOrDefaultForMode(spec, 'doc_action', 'doc_action_by_mode', mode, GetSpecDocRhs(spec, mode))
enddef

def GetSpecValueOrDefaultForMode(
  spec: dict<any>,
  singular_key: string,
  plural_key: string,
  mode: string,
  default_value: any,
): any
  if has_key(spec, plural_key)
    const values = spec[plural_key]
    if has_key(values, mode)
      return values[mode]
    endif
  endif

  if has_key(spec, singular_key)
    return spec[singular_key]
  endif

  return default_value
enddef

def GetSpecDocDesc(spec: dict<any>, mode: string): string
  return GetSpecValueOrDefaultForMode(spec, 'doc_desc', 'doc_desc_by_mode', mode, get(spec, 'desc', ''))
enddef

def GetSpecWhichKeyDesc(spec: dict<any>, mode: string): string
  return GetSpecValueOrDefaultForMode(spec, 'whichkey_desc', 'whichkey_desc_by_mode', mode, get(spec, 'desc', ''))
enddef

# ----------------------------------------------------------------------------
# 文档与动作规范化
# ----------------------------------------------------------------------------

def HumanizeDocLabel(text: string): string
  var value = text
  value = substitute(value, '[_-]', ' ', 'g')
  value = substitute(value, '[!+]', ' ', 'g')
  value = substitute(value, '\C\([a-z0-9]\)\([A-Z]\)', '\1 \2', 'g')
  value = substitute(value, '\<prev\>', 'previous', 'g')
  value = substitute(value, '\<goto\>', 'go to', 'g')
  value = substitute(value, '\<cmdline\>', 'command line', 'g')
  value = substitute(value, '\<codelens\>', 'code lens', 'g')
  value = substitute(value, '\s\+', ' ', 'g')
  return trim(value)
enddef

def LastNonOptionToken(tokens: list<string>): string
  var last = ''
  for token in tokens
    if token !~# '^-'
      last = token
    endif
  endfor
  return last
enddef

def NormalizeCommandSubject(text: string): string
  var value = text
  value = substitute(value, '^--preset=', '', '')
  value = HumanizeDocLabel(value)
  value = substitute(value, '^\.vim$', 'vim files', '')
  value = substitute(value, '^cmdhistory$', 'command history', '')
  value = substitute(value, '^mru$', 'recent files', '')
  value = substitute(value, '^gfiles$', 'git files', '')
  value = substitute(value, '^igrep$', 'interactive grep', '')
  value = substitute(value, '^expl Presets$', 'Explorer presets', '')
  value = substitute(value, '^coc Config$', 'config', '')
  value = substitute(value, '^vim lsp$', 'vim-lsp', '')
  value = substitute(value, '^coc ', 'CoC ', '')
  return value
enddef

def NormalizeExplorerPreset(preset: string): string
  if preset ==# 'buffer'
    return 'buffers'
  endif
  if preset ==# 'cocConfig'
    return 'config'
  endif
  if preset ==# '.vim'
    return 'vim files'
  endif
  return NormalizeCommandSubject(preset)
enddef

def BuildDocCommandAction(command: string, fallback_desc: string): string
  const tokens = split(command)
  if empty(tokens)
    return fallback_desc
  endif

  const head = tokens[0]

  if head ==# 'w'
    return 'write'
  endif

  if head ==# 'wq'
    return 'write and quit'
  endif

  if head ==# 'close'
    return 'close window'
  endif

  if head ==# 'bdelete'
    return 'delete buffer'
  endif

  if head ==# 'bnext'
    return 'next buffer'
  endif

  if head ==# 'bprevious'
    return 'previous buffer'
  endif

  if head ==# 'copen'
    return 'open quickfix'
  endif

  if head ==# 'cclose'
    return 'close quickfix'
  endif

  if head ==# 'cnext'
    return 'next quickfix item'
  endif

  if head ==# 'cprevious'
    return 'previous quickfix item'
  endif

  if head ==# 'source' && len(tokens) > 1 && tokens[1] ==# '$MYVIMRC'
    return 'reload vimrc'
  endif

  if head ==# 'Clap'
    return len(tokens) > 1 ? 'Clap ' .. NormalizeCommandSubject(tokens[1]) : 'Clap'
  endif

  if head ==# 'CocList'
    const target = LastNonOptionToken(tokens[1 :])
    return !empty(target) ? 'CoC list ' .. NormalizeCommandSubject(target) : 'CoC list'
  endif

  if head ==# 'CocListResume'
    return 'CoC list resume'
  endif

  if head ==# 'CocNext'
    return 'next CoC item'
  endif

  if head ==# 'CocPrev'
    return 'previous CoC item'
  endif

  if head ==# 'CocCommand' && len(tokens) > 1 && tokens[1] ==# 'explorer'
    const preset = LastNonOptionToken(tokens[2 :])
    return empty(preset) ? 'CoC Explorer' : 'CoC Explorer ' .. NormalizeExplorerPreset(preset)
  endif

  if head =~# '^Vista'
    if head ==# 'Vista!!'
      return 'Vista toggle'
    endif
    if head ==# 'Vista!'
      return 'Vista close'
    endif
    const target = len(tokens) > 1 ? tokens[1] : ''
    if target ==# 'finder!'
      return 'Vista finder all scopes'
    endif
    if target ==# 'info+'
      return 'Vista detailed info'
    endif
    if target ==# 'coc'
      return 'Vista CoC backend'
    endif
    if target ==# 'ctags'
      return 'Vista ctags backend'
    endif
    if target ==# 'vim_lsp'
      return 'Vista vim-lsp backend'
    endif
    const normalized_target = NormalizeCommandSubject(target)
    return empty(normalized_target) ? 'Vista' : 'Vista ' .. normalized_target
  endif

  if head =~# '^Floaterm'
    return 'Floaterm ' .. tolower(NormalizeCommandSubject(substitute(head, '^Floaterm', '', '')))
  endif

  if head ==# 'ToggleWhitespace'
    return 'whitespace toggle'
  endif

  if head ==# 'StripWhitespace'
    return 'whitespace cleanup'
  endif

  if head ==# 'WhichKey' || head ==# 'WhichKeyVisual'
    return 'Which-Key'
  endif

  if head ==# 'G'
    return 'Git status'
  endif

  if head ==# 'Git'
    const target = len(tokens) > 1 ? NormalizeCommandSubject(join(tokens[1 :], ' ')) : 'status'
    return 'Git ' .. target
  endif

  if head ==# 'Gdiffsplit'
    return 'Git diff'
  endif

  if head ==# 'Gwrite'
    return 'Git write'
  endif

  if head ==# 'Gread'
    return 'Git read'
  endif

  if head ==# 'Gclog'
    return 'Git commit log'
  endif

  if head ==# 'call'
    return fallback_desc
  endif

  return ':' .. command
enddef

def g:NormalizeDocAction(action: string, desc: string): string
  if action !~# '^:'
    return action
  endif

  const command = substitute(action, '^:', '', '')
  return BuildDocCommandAction(command, HumanizeDocLabel(desc))
enddef

def GetRequiredSpecValue(spec: dict<any>, singular_key: string, plural_key: string, mode: string): string
  if has_key(spec, plural_key)
    const values = spec[plural_key]
    if has_key(values, mode)
      return values[mode]
    endif
  endif

  if has_key(spec, singular_key)
    return spec[singular_key]
  endif

  throw $'Mapping spec missing "{singular_key}" or "{plural_key}": {string(spec)}'
enddef

def BuildMapCommand(mode: string, remap: bool): string
  const suffix = remap ? 'map' : 'noremap'
  return empty(mode) ? suffix : mode .. suffix
enddef

def GetOptionTokens(spec: dict<any>): list<string>
  var tokens: list<string> = []

  if get(spec, 'buffer', false)
    add(tokens, '<buffer>')
  endif
  if get(spec, 'silent', false)
    add(tokens, '<silent>')
  endif
  if get(spec, 'nowait', false)
    add(tokens, '<nowait>')
  endif
  if get(spec, 'expr', false)
    add(tokens, '<expr>')
  endif
  if get(spec, 'script', false)
    add(tokens, '<script>')
  endif
  if get(spec, 'unique', false)
    add(tokens, '<unique>')
  endif

  return tokens
enddef

def RegisterKeymapEntry(mode: string, spec: dict<any>)
  if !get(spec, 'doc', true)
    return
  endif

  EnsureKeymapRegistry()
  add(g:mcge_keymap_registry, BuildKeymapRegistryEntry(mode, spec))
enddef

def BuildKeymapRegistryEntry(mode: string, spec: dict<any>): dict<any>
  return {
    mode: mode,
    lhs: spec.lhs,
    action: GetSpecDocAction(spec, mode),
    rhs: GetSpecDocRhs(spec, mode),
    desc: GetSpecDocDesc(spec, mode),
    section: get(spec, 'doc_section', 'Other'),
  }
enddef

def PrepareMappedSpec(
  spec: dict<any>,
  singular_key: string,
  plural_key: string,
  Transform: func(string, string): string,
): dict<any>
  var prepared = copy(spec)
  const modes = GetSpecModes(spec)
  var rhs_by_mode: dict<string> = {}

  for mode in modes
    rhs_by_mode[mode] = Transform(GetRequiredSpecValue(spec, singular_key, plural_key, mode), mode)
  endfor

  prepared.rhs_by_mode = rhs_by_mode

  if has_key(prepared, singular_key)
    remove(prepared, singular_key)
  endif
  if has_key(prepared, plural_key)
    remove(prepared, plural_key)
  endif

  return prepared
enddef

def IsVisualCommandMode(mode: string): bool
  return index(['v', 'x', 's'], mode) >= 0
enddef

def BuildCommandPrefix(mode: string, clear_cmdline: bool): string
  var prefix = ':'
  if clear_cmdline
    prefix ..= '<C-u>'
  endif

  if mode ==# 'i'
    return '<Esc>' .. prefix
  endif
  if mode ==# 't'
    return '<C-\><C-n>' .. prefix
  endif
  if IsVisualCommandMode(mode) && !clear_cmdline
    return '<Esc>' .. prefix
  endif

  return prefix
enddef

def BuildCommandRhs(command: string, mode: string, clear_cmdline: bool): string
  return BuildCommandPrefix(mode, clear_cmdline) .. command .. '<CR>'
enddef

def NormalizePlug(plug: string): string
  return plug =~# '^<Plug>' ? plug : '<Plug>' .. plug
enddef

def NormalizeDocPlugAction(plug: string): string
  var action = plug
  if action =~# '^<Plug>'
    action = substitute(action, '^<Plug>', '', '')
  endif
  if strchars(action) >= 2
    const first = strcharpart(action, 0, 1)
    const last = strcharpart(action, strchars(action) - 1, 1)
    if first ==# '(' && last ==# ')'
      action = strcharpart(action, 1, strchars(action) - 2)
    endif
  endif
  return HumanizeDocLabel(action)
enddef

def ShouldRegisterWhichKey(spec: dict<any>): bool
  return has_key(spec, 'desc') || has_key(spec, 'whichkey_desc') || has_key(spec, 'whichkey_desc_by_mode')
enddef

def RegisterWhichKeySpec(spec: dict<any>, modes: list<string>)
  if !ShouldRegisterWhichKey(spec)
    return
  endif

  for mode in modes
    RegisterWhichKeyDescription(
      get(spec, 'prefix', '<Space>'),
      spec.lhs,
      GetSpecWhichKeyDesc(spec, mode),
      [mode],
    )
  endfor
enddef

# ----------------------------------------------------------------------------
# 映射注册
# ----------------------------------------------------------------------------

def g:Map(spec: dict<any>)
  const modes = GetSpecModes(spec)
  const option_tokens = GetOptionTokens(spec)
  const remap = get(spec, 'remap', false)

  for mode in modes
    var parts = [BuildMapCommand(mode, remap)]
    if !empty(option_tokens)
      add(parts, join(option_tokens, ''))
    endif
    add(parts, spec.lhs)
    add(parts, GetSpecRhs(spec, mode))
    execute join(parts, ' ')
    RegisterKeymapEntry(mode, spec)
  endfor

  RegisterWhichKeySpec(spec, modes)
enddef

def g:MapMany(specs: list<dict<any>>, defaults: dict<any> = {})
  for spec in specs
    g:Map(MergeMapSpec(defaults, spec))
  endfor
enddef

def g:CmdMap(spec: dict<any>)
  var prepared = PrepareMappedSpec(
    spec,
    'cmd',
    'cmd_by_mode',
    (command, mode) => BuildCommandRhs(
      command,
      mode,
        GetSpecValueOrDefaultForMode(spec, 'clear_cmdline', 'clear_cmdline_by_mode', mode, true),
    ),
  )
  var doc_rhs_by_mode: dict<string> = {}
  var doc_action_by_mode: dict<string> = {}
  for mode in GetSpecModes(spec)
    const command = GetRequiredSpecValue(spec, 'cmd', 'cmd_by_mode', mode)
    doc_rhs_by_mode[mode] = ':' .. command
    doc_action_by_mode[mode] = BuildDocCommandAction(command, HumanizeDocLabel(GetSpecDocDesc(spec, mode)))
  endfor
  prepared.doc_rhs_by_mode = doc_rhs_by_mode
  if !has_key(prepared, 'doc_action') && !has_key(prepared, 'doc_action_by_mode')
    prepared.doc_action_by_mode = doc_action_by_mode
  endif
  g:Map(prepared)
enddef

def g:CmdMapMany(specs: list<dict<any>>, defaults: dict<any> = {})
  for spec in specs
    g:CmdMap(MergeMapSpec(defaults, spec))
  endfor
enddef

def g:PlugMap(spec: dict<any>)
  var prepared = PrepareMappedSpec(
    spec,
    'plug',
    'plug_by_mode',
    (plug, _) => NormalizePlug(plug),
  )
  var doc_rhs_by_mode: dict<string> = {}
  var doc_action_by_mode: dict<string> = {}
  const desc_action = HumanizeDocLabel(get(spec, 'desc', ''))
  for mode in GetSpecModes(spec)
    const plug = GetRequiredSpecValue(spec, 'plug', 'plug_by_mode', mode)
    doc_rhs_by_mode[mode] = NormalizePlug(plug)
    doc_action_by_mode[mode] = !empty(desc_action) ? desc_action : NormalizeDocPlugAction(plug)
  endfor
  prepared.doc_rhs_by_mode = doc_rhs_by_mode
  if !has_key(prepared, 'doc_action') && !has_key(prepared, 'doc_action_by_mode')
    prepared.doc_action_by_mode = doc_action_by_mode
  endif
  if !has_key(prepared, 'remap')
    prepared.remap = true
  endif
  g:Map(prepared)
enddef

def g:PlugMapMany(specs: list<dict<any>>, defaults: dict<any> = {})
  for spec in specs
    g:PlugMap(MergeMapSpec(defaults, spec))
  endfor
enddef

# vim: set ft=vim sw=2 ts=2 sts=2 et:
