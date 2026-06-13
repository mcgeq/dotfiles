vim9script
# ============================================================================
# 组件: Core / Workspace
# 作者: mcge <mcgeq@outlook.com>
# 说明: 提供共享的项目根目录、会话命名与终端工作目录辅助。
# ============================================================================

const PROJECT_ROOT_MARKERS = [
  '.git',
  '.hg',
  '.jj',
  '.svn',
  'pnpm-workspace.yaml',
  'package.json',
  'tsconfig.json',
  'jsconfig.json',
  'pyproject.toml',
  'uv.lock',
  'requirements.txt',
  'Cargo.toml',
  'rust-project.json',
  'go.work',
  'go.mod',
  'build.zig',
  'zls.json',
  'CMakeLists.txt',
  'Makefile',
]

def ParentDir(path: string): string
  return fnamemodify(path, ':h')
enddef

def IsRootPath(path: string): bool
  return empty(path) || ParentDir(path) ==# path
enddef

def EnsureNormalizedDir(path: string): string
  if empty(path)
    return g:NormalizeComparablePath(getcwd())
  endif

  var absolute = path
  if !isdirectory(absolute)
    absolute = fnamemodify(path, ':p:h')
  endif
  return g:NormalizeComparablePath(absolute)
enddef

def SessionDir(): string
  return g:DATA_DIR .. '/sessions'
enddef

def SessionPathForName(name: string): string
  return SessionDir() .. '/' .. name
enddef

def CurrentBufferPath(): string
  const filepath = expand('%:p')
  return filereadable(filepath) || isdirectory(filepath) ? g:NormalizeComparablePath(filepath) : ''
enddef

def CurrentBufferDir(): string
  const filepath = CurrentBufferPath()
  if empty(filepath)
    return g:NormalizeComparablePath(getcwd())
  endif
  return g:NormalizeComparablePath(fnamemodify(filepath, ':h'))
enddef

def MarkerPath(dir_path: string, marker: string): string
  return dir_path .. '/' .. marker
enddef

def HasProjectMarker(dir_path: string): bool
  for marker in PROJECT_ROOT_MARKERS
    const candidate = MarkerPath(dir_path, marker)
    if filereadable(candidate) || isdirectory(candidate)
      return true
    endif
  endfor
  return false
enddef

def DetectProjectRoot(start_path: string): string
  var dir_path = EnsureNormalizedDir(start_path)

  while !empty(dir_path)
    if HasProjectMarker(dir_path)
      return dir_path
    endif

    const parent = ParentDir(dir_path)
    if parent ==# dir_path
      break
    endif
    dir_path = parent
  endwhile

  return ''
enddef

def SessionSlug(root: string): string
  var normalized = g:NormalizeComparablePath(root)
  normalized = substitute(normalized, '^[/\\]\+', '', '')
  normalized = substitute(normalized, '[:/\\]\+', '__', 'g')
  normalized = substitute(normalized, '\s\+', '_', 'g')
  normalized = substitute(normalized, '[^0-9A-Za-z._-]\+', '_', 'g')
  normalized = substitute(normalized, '_\+', '_', 'g')
  normalized = substitute(normalized, '^_\+', '', '')
  normalized = substitute(normalized, '_\+$', '', '')

  if empty(normalized)
    return 'workspace.vim'
  endif
  return normalized .. '.vim'
enddef

def ReadSessionItems(): list<dict<any>>
  var items: list<dict<any>> = []
  const session_dir = SessionDir()

  if !isdirectory(session_dir)
    return items
  endif

  for session_path in sort(globpath(session_dir, '*', 0, 1))
    if isdirectory(session_path)
      continue
    endif

    const name = fnamemodify(session_path, ':t')
    if name ==# '__LAST__'
      continue
    endif

    add(items, {
      name: name,
      path: session_path,
      root: SessionRootFromName(name),
      mtime: getftime(session_path),
    })
  endfor

  return sort(items, (_, left, right) => left.mtime == right.mtime ? (left.name > right.name ? 1 : -1) : right.mtime - left.mtime)
enddef

def SessionRootFromName(name: string): string
  var bare = substitute(name, '\.vim$', '', '')
  if empty(bare)
    return ''
  endif

  var root = substitute(bare, '__', '/', 'g')
  if g:IS_WINDOWS && root =~? '^[a-z]/'
    root = toupper(root[0]) .. ':' .. root[1 : ]
  elseif root !~# '^/'
    root = '/' .. root
  endif

  return g:NormalizeComparablePath(root)
enddef

def SelectFromItems(prompt: string, items: list<dict<any>>, LabelFn: func(dict<any>): string): dict<any>
  if empty(items)
    return {}
  endif

  var choices = [prompt]
  for idx in range(len(items))
    add(choices, printf('%d. %s', idx + 1, LabelFn(items[idx])))
  endfor

  const choice = inputlist(choices)
  if choice <= 0 || choice > len(items)
    return {}
  endif

  return items[choice - 1]
enddef

def g:WorkspaceProjectRoot(...args: list<any>): string
  const target = len(args) > 0 ? string(args[0]) : CurrentBufferDir()
  return DetectProjectRoot(target)
enddef

def g:WorkspaceCurrentRoot(): string
  const buffer_root = g:WorkspaceProjectRoot(CurrentBufferDir())
  if !empty(buffer_root)
    return buffer_root
  endif

  const cwd_root = g:WorkspaceProjectRoot(getcwd())
  if !empty(cwd_root)
    return cwd_root
  endif

  return g:NormalizeComparablePath(getcwd())
enddef

def g:WorkspaceCurrentBufferDir(): string
  return CurrentBufferDir()
enddef

def g:WorkspaceCurrentSessionName(...args: list<any>): string
  const root = len(args) > 0 ? string(args[0]) : g:WorkspaceCurrentRoot()
  return SessionSlug(root)
enddef

def g:WorkspaceCurrentSessionPath(...args: list<any>): string
  return SessionPathForName(g:WorkspaceCurrentSessionName(len(args) > 0 ? string(args[0]) : g:WorkspaceCurrentRoot()))
enddef

def g:WorkspaceSessionExists(...args: list<any>): bool
  return filereadable(g:WorkspaceCurrentSessionPath(len(args) > 0 ? string(args[0]) : g:WorkspaceCurrentRoot()))
enddef

def g:WorkspaceShowInfo()
  const cwd = g:NormalizeComparablePath(getcwd())
  const buffer_dir = CurrentBufferDir()
  const buffer_root = g:WorkspaceProjectRoot(buffer_dir)
  const cwd_root = g:WorkspaceProjectRoot(cwd)
  const active_root = g:WorkspaceCurrentRoot()
  const session_name = g:WorkspaceCurrentSessionName(active_root)
  const session_path = g:WorkspaceCurrentSessionPath(active_root)

  echo '=========================================='
  echo 'Workspace Information'
  echo '=========================================='
  echo '  Cwd:         ' .. cwd
  echo '  Cwd root:    ' .. (empty(cwd_root) ? 'n/a' : cwd_root)
  echo '  Buffer dir:  ' .. buffer_dir
  echo '  Buffer root: ' .. (empty(buffer_root) ? 'n/a' : buffer_root)
  echo '  Active root: ' .. active_root
  echo '  Session:     ' .. session_name
  echo '  Saved:       ' .. (filereadable(session_path) ? 'yes' : 'no')
  echo '=========================================='
enddef

def g:WorkspaceCdToCurrentRoot()
  const root = g:WorkspaceCurrentRoot()
  execute 'cd ' .. fnameescape(root)
  g:ErrInfo('Project root -> ' .. root)
enddef

def g:WorkspaceSelectProject()
  var roots: dict<bool> = {}
  const current_root = g:WorkspaceCurrentRoot()
  roots[current_root] = true

  for item in ReadSessionItems()
    if !empty(item.root)
      roots[item.root] = true
    endif
  endfor

  var items: list<dict<any>> = []
  for root in sort(keys(roots))
    add(items, {
      root: root,
      current: root ==# current_root,
      has_session: filereadable(g:WorkspaceCurrentSessionPath(root)),
    })
  endfor

  const selected = SelectFromItems('Projects', items, (item) => printf('%s%s%s', fnamemodify(item.root, ':t'), item.current ? ' [current]' : '', item.has_session ? ' [session]' : ''))
  if empty(selected)
    return
  endif

  execute 'cd ' .. fnameescape(selected.root)
  g:ErrInfo('Project root -> ' .. selected.root)
enddef

def g:WorkspaceSaveSession()
  const root = g:WorkspaceCurrentRoot()
  execute 'SSave! ' .. fnameescape(g:WorkspaceCurrentSessionName(root))
enddef

def g:WorkspaceLoadSession()
  const root = g:WorkspaceCurrentRoot()
  const name = g:WorkspaceCurrentSessionName(root)
  if !filereadable(SessionPathForName(name))
    g:ErrWarn('No saved workspace session yet for: ' .. root)
    return
  endif
  execute 'SLoad ' .. fnameescape(name)
enddef

def g:WorkspaceDeleteSession()
  const root = g:WorkspaceCurrentRoot()
  const name = g:WorkspaceCurrentSessionName(root)
  if !filereadable(SessionPathForName(name))
    g:ErrWarn('No saved workspace session to delete for: ' .. root)
    return
  endif
  execute 'SDelete! ' .. fnameescape(name)
enddef

def g:WorkspaceSelectSession(action: string = 'load')
  const items = ReadSessionItems()
  if empty(items)
    g:ErrWarn('No saved workspace sessions yet.')
    return
  endif

  const selected = SelectFromItems(
    action ==# 'delete' ? 'Delete session' : 'Load session',
    items,
    (item) => printf('%s  %s', fnamemodify(item.root, ':t'), item.root)
  )
  if empty(selected)
    return
  endif

  if action ==# 'delete'
    execute 'SDelete! ' .. fnameescape(selected.name)
    return
  endif

  execute 'SLoad ' .. fnameescape(selected.name)
enddef

def g:WorkspaceSelectProjectSession()
  var items: list<dict<any>> = []
  for item in ReadSessionItems()
    add(items, item)
  endfor

  if empty(items)
    g:ErrWarn('No saved workspace sessions yet.')
    return
  endif

  const selected = SelectFromItems('Projects + sessions', items, (item) => printf('%s  %s', fnamemodify(item.root, ':t'), item.root))
  if empty(selected)
    return
  endif

  execute 'cd ' .. fnameescape(selected.root)
  execute 'SLoad ' .. fnameescape(selected.name)
enddef

def g:WorkspaceOpenTerminal(mode: string)
  var cwd = g:NormalizeComparablePath(getcwd())

  if mode ==# 'project'
    cwd = g:WorkspaceCurrentRoot()
  elseif mode ==# 'buffer'
    cwd = CurrentBufferDir()
  endif

  execute 'FloatermNew --cwd=' .. g:EscapeFloatermArg(cwd)
enddef

command! -nargs=0 ProjectInfo call g:WorkspaceShowInfo()
command! -nargs=0 ProjectRoot call g:WorkspaceCdToCurrentRoot()
command! -nargs=0 ProjectSelect call g:WorkspaceSelectProject()
command! -nargs=0 ProjectSessionSelect call g:WorkspaceSelectProjectSession()

command! -nargs=0 SessionInfo call g:WorkspaceShowInfo()
command! -nargs=0 SessionSave call g:WorkspaceSaveSession()
command! -nargs=0 SessionLoad call g:WorkspaceLoadSession()
command! -nargs=0 SessionSelect call g:WorkspaceSelectSession('load')
command! -nargs=0 SessionDelete call g:WorkspaceDeleteSession()
command! -nargs=0 SessionSelectDelete call g:WorkspaceSelectSession('delete')

command! -nargs=0 TerminalCwd call g:WorkspaceOpenTerminal('cwd')
command! -nargs=0 TerminalProject call g:WorkspaceOpenTerminal('project')
command! -nargs=0 TerminalBufferDir call g:WorkspaceOpenTerminal('buffer')

# vim: set ft=vim sw=2 ts=2 sts=2 et:
