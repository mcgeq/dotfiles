vim9script
# ============================================================================
# 组件: Core / Runner
# 作者: mcge <mcgeq@outlook.com>
# 说明: 提供统一的 build/run/test 工作流入口，复用 Floaterm 作为执行后端。
# ============================================================================

const FRONTEND_FILETYPES = [
  'javascript',
  'javascriptreact',
  'typescript',
  'typescriptreact',
  'vue',
]

const SUPPORTED_FILETYPES = [
  'rust',
  'zig',
  'go',
  'python',
  'javascript',
  'javascriptreact',
  'typescript',
  'typescriptreact',
  'vue',
]

const FRONTEND_LOCKFILES = [
  'pnpm-lock.yaml',
  'bun.lockb',
  'bun.lock',
  'yarn.lock',
  'package-lock.json',
]

const PYTHON_VENV_MARKERS = g:IS_WINDOWS
  ? ['.venv/Scripts/python.exe', 'venv/Scripts/python.exe']
  : ['.venv/bin/python', 'venv/bin/python']

var config = {
  enabled: true,
  supported_filetypes: SUPPORTED_FILETYPES,
}

var last_run: dict<any> = {}

def AbsolutePath(path: string): string
  return g:NormalizePath(fnamemodify(path, ':p'))
enddef

def PathExists(path: string): bool
  return filereadable(path) || isdirectory(path)
enddef

def CurrentFilePath(): string
  const path = expand('%:p')
  return empty(path) ? '' : AbsolutePath(path)
enddef

def CurrentFileDir(): string
  const path = CurrentFilePath()
  return empty(path) ? AbsolutePath(getcwd()) : AbsolutePath(fnamemodify(path, ':h'))
enddef

def ParentDir(path: string): string
  return fnamemodify(path, ':h')
enddef

def FindUpwardPath(start_dir: string, names: list<string>): string
  if empty(start_dir)
    return ''
  endif

  var dir_path = AbsolutePath(start_dir)
  while !empty(dir_path)
    for name in names
      const candidate = dir_path .. '/' .. name
      if PathExists(candidate)
        return AbsolutePath(candidate)
      endif
    endfor

    const parent = ParentDir(dir_path)
    if parent ==# dir_path
      break
    endif
    dir_path = parent
  endwhile

  return ''
enddef

def FindUpwardDir(start_dir: string, names: list<string>): string
  const path = FindUpwardPath(start_dir, names)
  return empty(path) ? '' : AbsolutePath(fnamemodify(path, ':h'))
enddef

def FirstExecutable(names: list<string>): string
  for name in names
    const path = exepath(name)
    if !empty(path)
      return path
    endif
    if executable(name)
      return name
    endif
  endfor
  return ''
enddef

def PowerShellQuote(value: string): string
  return "'" .. substitute(value, "'", "''", 'g') .. "'"
enddef

def ShellQuote(value: string): string
  return g:IS_WINDOWS ? PowerShellQuote(value) : shellescape(value)
enddef

def FrontendFiletype(filetype: string): bool
  return index(FRONTEND_FILETYPES, filetype) >= 0
enddef

def SupportedFiletype(filetype: string): bool
  return index(config.supported_filetypes, filetype) >= 0
enddef

def BuildTerminalSpec(cmd: string, cwd: string, title: string): dict<any>
  return {
    kind: 'terminal',
    cmd: cmd,
    cwd: AbsolutePath(cwd),
    title: title,
  }
enddef

def BuildPathsSpec(paths: list<string>, title: string): dict<any>
  return {
    kind: 'paths',
    paths: paths,
    title: title,
  }
enddef

def BuildErrorSpec(message: string): dict<any>
  return {
    kind: 'error',
    message: message,
  }
enddef

def EnsureRunnerReady(action_name: string): bool
  if empty(CurrentFilePath())
    g:ErrWarn('Runner requires a saved file for ' .. action_name .. '.')
    return false
  endif

  if &modified
    try
      silent update
    catch
      g:ErrError('Failed to save buffer before ' .. action_name .. ': ' .. v:exception)
      return false
    endtry
  endif

  return true
enddef

def RemovePaths(paths: list<string>, title: string)
  var removed: list<string> = []

  for path in paths
    if !PathExists(path)
      continue
    endif

    if isdirectory(path)
      delete(path, 'rf')
    else
      delete(path)
    endif

    if PathExists(path)
      g:ErrWarn(title .. ' clean failed for: ' .. path)
      return
    endif
    add(removed, path)
  endfor

  if empty(removed)
    g:ErrInfo(title .. ': nothing to clean.')
    return
  endif

  g:ErrInfo(title .. ': cleaned ' .. join(removed, ', '))
enddef

def OpenRunnerTerminal(spec: dict<any>)
  if !g:CommandExists('FloatermNew')
    g:ErrWarn('Floaterm is unavailable. Runner commands require :FloatermNew.')
    return
  endif

  var command = 'FloatermNew! --cwd=' .. g:EscapeFloatermArg(spec.cwd)
  if has_key(spec, 'title') && !empty(spec.title)
    command ..= ' --title=' .. g:EscapeFloatermArg(spec.title)
  endif
  command ..= ' ' .. spec.cmd
  execute command
enddef

def ExecuteRunnerSpec(spec: dict<any>, remember: bool = true): bool
  const kind = get(spec, 'kind', 'error')

  if kind ==# 'error'
    g:ErrWarn(get(spec, 'message', 'Runner action is unavailable.'))
    return false
  endif

  if kind ==# 'paths'
    RemovePaths(get(spec, 'paths', []), get(spec, 'title', 'Runner'))
    return true
  endif

  if kind ==# 'terminal'
    if remember
      last_run = deepcopy(spec)
    endif
    OpenRunnerTerminal(spec)
    return true
  endif

  g:ErrWarn('Runner returned an unknown action kind.')
  return false
enddef

def LocalPythonPath(start_dir: string): string
  return FindUpwardPath(start_dir, PYTHON_VENV_MARKERS)
enddef

def PythonProjectRoot(start_dir: string): string
  return FindUpwardDir(start_dir, ['uv.lock', 'pyproject.toml', 'requirements.txt'])
enddef

def UseUv(root: string): bool
  return !empty(root)
    && executable('uv')
    && (PathExists(root .. '/uv.lock') || PathExists(root .. '/pyproject.toml'))
enddef

def UseTy(root: string): bool
  return executable('ty') && (!empty(root) || !empty(CurrentFilePath()))
enddef

def PackageRoot(start_dir: string): string
  return FindUpwardDir(start_dir, ['package.json'])
enddef

def ReadJsonFile(path: string): dict<any>
  if !filereadable(path)
    return {}
  endif

  try
    const decoded = json_decode(join(readfile(path), "\n"))
    if type(decoded) == v:t_dict
      return decoded
    endif
  catch
    return {}
  endtry

  return {}
enddef

def PackageScripts(root: string): dict<any>
  if empty(root)
    return {}
  endif

  const data = ReadJsonFile(root .. '/package.json')
  const scripts = get(data, 'scripts', {})
  return type(scripts) == v:t_dict ? scripts : {}
enddef

def ScriptValue(scripts: dict<any>, name: string): string
  if !has_key(scripts, name) || type(scripts[name]) != v:t_string
    return ''
  endif
  return string(scripts[name])
enddef

def DetectPackageManager(root: string): string
  const lock_path = FindUpwardPath(root, FRONTEND_LOCKFILES)

  if !empty(lock_path)
    const name = fnamemodify(lock_path, ':t')
    if name ==# 'pnpm-lock.yaml' && executable('pnpm')
      return 'pnpm'
    endif
    if (name ==# 'bun.lockb' || name ==# 'bun.lock') && executable('bun')
      return 'bun'
    endif
    if name ==# 'yarn.lock' && executable('yarn')
      return 'yarn'
    endif
    if name ==# 'package-lock.json' && executable('npm')
      return 'npm'
    endif
  endif

  if executable('pnpm')
    return 'pnpm'
  endif
  if executable('bun')
    return 'bun'
  endif
  if executable('yarn')
    return 'yarn'
  endif
  if executable('npm')
    return 'npm'
  endif

  return ''
enddef

def PackageManagerCommand(manager: string, script_name: string): string
  if manager ==# 'bun'
    return 'bun run ' .. script_name
  endif
  if manager ==# 'yarn'
    return 'yarn run ' .. script_name
  endif
  if manager ==# 'pnpm'
    return 'pnpm run ' .. script_name
  endif
  return 'npm run ' .. script_name
enddef

def FrontendRunScript(scripts: dict<any>): string
  for script_name in ['dev', 'start', 'run']
    const value = ScriptValue(scripts, script_name)
    if !empty(value)
      return script_name
    endif
  endfor
  return ''
enddef

def RustSpec(action: string): dict<any>
  const file = CurrentFilePath()
  const dir = CurrentFileDir()
  const root = FindUpwardDir(dir, ['Cargo.toml'])

  if !empty(root)
    if action ==# 'build'
      return BuildTerminalSpec('cargo build', root, 'Runner Rust')
    endif
    if action ==# 'run'
      return BuildTerminalSpec('cargo run', root, 'Runner Rust')
    endif
    if action ==# 'test'
      return BuildTerminalSpec('cargo test', root, 'Runner Rust')
    endif
    if action ==# 'clean'
      return BuildTerminalSpec('cargo clean', root, 'Runner Rust')
    endif
  endif

  const rustc = FirstExecutable(['rustc'])
  if empty(rustc)
    return BuildErrorSpec('No `rustc` executable found on PATH.')
  endif

  const stem = fnamemodify(file, ':t:r')
  const exe_suffix = g:IS_WINDOWS ? '.exe' : ''
  const output = dir .. '/' .. stem .. exe_suffix
  const test_output = dir .. '/' .. stem .. '_test' .. exe_suffix
  const compile_cmd = ShellQuote(rustc) .. ' ' .. ShellQuote(file)

  if action ==# 'build'
    return BuildTerminalSpec(compile_cmd .. ' -o ' .. ShellQuote(output), dir, 'Runner Rust')
  endif
  if action ==# 'run'
    const built = compile_cmd .. ' -o ' .. ShellQuote(output)
    const run_cmd = g:IS_WINDOWS
      ? built .. '; if ($LASTEXITCODE -eq 0) { & ' .. PowerShellQuote(output) .. ' }'
      : built .. ' && ' .. ShellQuote(output)
    return BuildTerminalSpec(run_cmd, dir, 'Runner Rust')
  endif
  if action ==# 'test'
    const built = ShellQuote(rustc) .. ' --test ' .. ShellQuote(file) .. ' -o ' .. ShellQuote(test_output)
    const test_cmd = g:IS_WINDOWS
      ? built .. '; if ($LASTEXITCODE -eq 0) { & ' .. PowerShellQuote(test_output) .. ' }'
      : built .. ' && ' .. ShellQuote(test_output)
    return BuildTerminalSpec(test_cmd, dir, 'Runner Rust')
  endif
  if action ==# 'clean'
    return BuildPathsSpec([output, test_output], 'Runner Rust')
  endif

  return BuildErrorSpec('Rust runner does not support action: ' .. action)
enddef

def ZigSpec(action: string): dict<any>
  const file = CurrentFilePath()
  const dir = CurrentFileDir()
  const root = FindUpwardDir(dir, ['build.zig'])

  if !empty(root)
    if action ==# 'build'
      return BuildTerminalSpec('zig build', root, 'Runner Zig')
    endif
    if action ==# 'run'
      return BuildTerminalSpec('zig build run', root, 'Runner Zig')
    endif
    if action ==# 'test'
      return BuildTerminalSpec('zig build test', root, 'Runner Zig')
    endif
    if action ==# 'clean'
      return BuildErrorSpec('Zig project clean is not configured. `zig build` has no universal clean command.')
    endif
  endif

  const zig = FirstExecutable(['zig'])
  if empty(zig)
    return BuildErrorSpec('No `zig` executable found on PATH.')
  endif

  const stem = fnamemodify(file, ':t:r')
  const exe_suffix = g:IS_WINDOWS ? '.exe' : ''
  const output = dir .. '/' .. stem .. exe_suffix

  if action ==# 'build'
    return BuildTerminalSpec(ShellQuote(zig) .. ' build-exe ' .. ShellQuote(file), dir, 'Runner Zig')
  endif
  if action ==# 'run'
    return BuildTerminalSpec(ShellQuote(zig) .. ' run ' .. ShellQuote(file), dir, 'Runner Zig')
  endif
  if action ==# 'test'
    return BuildTerminalSpec(ShellQuote(zig) .. ' test ' .. ShellQuote(file), dir, 'Runner Zig')
  endif
  if action ==# 'clean'
    return BuildPathsSpec([output], 'Runner Zig')
  endif

  return BuildErrorSpec('Zig runner does not support action: ' .. action)
enddef

def GoSpec(action: string): dict<any>
  const file = CurrentFilePath()
  const dir = CurrentFileDir()
  const root = FindUpwardDir(dir, ['go.work', 'go.mod'])

  if action ==# 'build'
    if !empty(root) && PathExists(root .. '/go.mod')
      return BuildTerminalSpec('go build .', dir, 'Runner Go')
    endif
    return BuildTerminalSpec('go build ' .. ShellQuote(file), dir, 'Runner Go')
  endif
  if action ==# 'run'
    if !empty(root) && PathExists(root .. '/go.mod')
      return BuildTerminalSpec('go run .', dir, 'Runner Go')
    endif
    return BuildTerminalSpec('go run ' .. ShellQuote(file), dir, 'Runner Go')
  endif
  if action ==# 'test'
    return BuildTerminalSpec('go test', dir, 'Runner Go')
  endif
  if action ==# 'clean'
    if !empty(root) && PathExists(root .. '/go.mod')
      return BuildTerminalSpec('go clean', dir, 'Runner Go')
    endif
    return BuildErrorSpec('Single-file Go clean is not configured.')
  endif

  return BuildErrorSpec('Go runner does not support action: ' .. action)
enddef

def PythonSpec(action: string): dict<any>
  const file = CurrentFilePath()
  const dir = CurrentFileDir()
  const root = PythonProjectRoot(dir)
  const use_uv = UseUv(root)
  const use_ty = UseTy(root)
  const local_python = LocalPythonPath(dir)
  const python = !empty(local_python) ? local_python : FirstExecutable(['python', 'python3'])
  const cwd = !empty(root) ? root : dir
  const cache_dir = dir .. '/__pycache__'

  if action ==# 'clean'
    return BuildPathsSpec([cache_dir], 'Runner Python')
  endif

  if action ==# 'build'
    if use_ty && use_uv
      return BuildTerminalSpec('uv run ty check ' .. ShellQuote(file), cwd, 'Runner Python')
    endif
    if use_ty
      return BuildTerminalSpec('ty check ' .. ShellQuote(file), cwd, 'Runner Python')
    endif
    if use_uv
      return BuildTerminalSpec('uv run python -m py_compile ' .. ShellQuote(file), cwd, 'Runner Python')
    endif
    if empty(python)
      return BuildErrorSpec('No Python executable found on PATH.')
    endif
    return BuildTerminalSpec(ShellQuote(python) .. ' -m py_compile ' .. ShellQuote(file), cwd, 'Runner Python')
  endif

  if use_uv
    if action ==# 'run'
      return BuildTerminalSpec('uv run python ' .. ShellQuote(file), cwd, 'Runner Python')
    endif
    if action ==# 'test'
      return BuildTerminalSpec('uv run pytest', cwd, 'Runner Python')
    endif
    if action ==# 'test_file'
      return BuildTerminalSpec('uv run pytest ' .. ShellQuote(file), cwd, 'Runner Python')
    endif
  endif

  if empty(python)
    return BuildErrorSpec('No Python executable found on PATH.')
  endif

  if action ==# 'run'
    return BuildTerminalSpec(ShellQuote(python) .. ' ' .. ShellQuote(file), cwd, 'Runner Python')
  endif
  if action ==# 'test'
    return BuildTerminalSpec(ShellQuote(python) .. ' -m pytest', cwd, 'Runner Python')
  endif
  if action ==# 'test_file'
    return BuildTerminalSpec(ShellQuote(python) .. ' -m pytest ' .. ShellQuote(file), cwd, 'Runner Python')
  endif

  return BuildErrorSpec('Python runner does not support action: ' .. action)
enddef

def FrontendSpec(action: string): dict<any>
  const file = CurrentFilePath()
  const dir = CurrentFileDir()
  const root = PackageRoot(dir)

  if empty(root)
    return BuildErrorSpec('Frontend runner requires a nearby package.json.')
  endif

  const scripts = PackageScripts(root)
  const manager = DetectPackageManager(root)

  if empty(manager)
    return BuildErrorSpec('No supported package manager found on PATH. Expected pnpm, bun, yarn, or npm.')
  endif

  if action ==# 'run'
    const script_name = FrontendRunScript(scripts)
    if empty(script_name)
      return BuildErrorSpec('No `dev`, `start`, or `run` script found in package.json.')
    endif
    return BuildTerminalSpec(PackageManagerCommand(manager, script_name), root, 'Runner Frontend')
  endif

  if action ==# 'build'
    if empty(ScriptValue(scripts, 'build'))
      return BuildErrorSpec('No `build` script found in package.json.')
    endif
    return BuildTerminalSpec(PackageManagerCommand(manager, 'build'), root, 'Runner Frontend')
  endif

  if action ==# 'test'
    if empty(ScriptValue(scripts, 'test'))
      return BuildErrorSpec('No `test` script found in package.json.')
    endif
    return BuildTerminalSpec(PackageManagerCommand(manager, 'test'), root, 'Runner Frontend')
  endif

  if action ==# 'test_file'
    const test_script = ScriptValue(scripts, 'test')
    if empty(test_script)
      return BuildErrorSpec('No `test` script found in package.json.')
    endif
    if test_script =~? '\<vitest\>' || test_script =~? '\<jest\>'
      return BuildTerminalSpec(PackageManagerCommand(manager, 'test') .. ' -- ' .. ShellQuote(file), root, 'Runner Frontend')
    endif
    return BuildErrorSpec('Current `test` script is not recognized as a file-filterable runner.')
  endif

  if action ==# 'clean'
    if empty(ScriptValue(scripts, 'clean'))
      return BuildErrorSpec('No `clean` script found in package.json.')
    endif
    return BuildTerminalSpec(PackageManagerCommand(manager, 'clean'), root, 'Runner Frontend')
  endif

  return BuildErrorSpec('Frontend runner does not support action: ' .. action)
enddef

def RunnerSpec(filetype: string, action: string): dict<any>
  if filetype ==# 'rust'
    return RustSpec(action)
  endif
  if filetype ==# 'zig'
    return ZigSpec(action)
  endif
  if filetype ==# 'go'
    return GoSpec(action)
  endif
  if filetype ==# 'python'
    return PythonSpec(action)
  endif
  if FrontendFiletype(filetype)
    return FrontendSpec(action)
  endif
  return BuildErrorSpec('Runner currently supports rust, zig, go, python, javascript, typescript, and vue buffers.')
enddef

def RunnerModeLabel(filetype: string): string
  const dir = CurrentFileDir()

  if filetype ==# 'rust'
    return empty(FindUpwardDir(dir, ['Cargo.toml'])) ? 'single file' : 'Cargo project'
  endif
  if filetype ==# 'zig'
    return empty(FindUpwardDir(dir, ['build.zig'])) ? 'single file' : 'Zig project'
  endif
  if filetype ==# 'go'
    return empty(FindUpwardDir(dir, ['go.work', 'go.mod'])) ? 'single file' : 'Go module'
  endif
  if filetype ==# 'python'
    const root = PythonProjectRoot(dir)
    if UseUv(root) && UseTy(root)
      return 'uv + ty'
    endif
    if UseUv(root)
      return 'uv'
    endif
    if !empty(LocalPythonPath(dir))
      return 'virtualenv'
    endif
    return 'system python'
  endif
  if FrontendFiletype(filetype)
    const root = PackageRoot(dir)
    if empty(root)
      return 'no package.json'
    endif
    const manager = DetectPackageManager(root)
    return empty(manager) ? 'package.json project' : manager .. ' project'
  endif

  return 'unsupported'
enddef

def RunnerRootLabel(filetype: string): string
  const dir = CurrentFileDir()

  if filetype ==# 'rust'
    return FindUpwardDir(dir, ['Cargo.toml'])
  endif
  if filetype ==# 'zig'
    return FindUpwardDir(dir, ['build.zig'])
  endif
  if filetype ==# 'go'
    return FindUpwardDir(dir, ['go.work', 'go.mod'])
  endif
  if filetype ==# 'python'
    return PythonProjectRoot(dir)
  endif
  if FrontendFiletype(filetype)
    return PackageRoot(dir)
  endif

  return ''
enddef

def RunnerActions(filetype: string): list<string>
  if filetype ==# 'python' || FrontendFiletype(filetype)
    return ['build', 'run', 'test', 'test-file', 'repeat', 'clean']
  endif
  if SupportedFiletype(filetype)
    return ['build', 'run', 'test', 'repeat', 'clean']
  endif
  return []
enddef

def RunnerNote(filetype: string): string
  if filetype ==# 'python'
    return 'build prefers ty and uses uv when a Python project is detected'
  endif
  if FrontendFiletype(filetype)
    return 'run prefers `dev`, then `start`, then `run` from package.json'
  endif
  return ''
enddef

def g:RunnerShowInfo()
  const filetype = &filetype
  const filepath = CurrentFilePath()

  if !SupportedFiletype(filetype)
    g:ErrInfo('Runner currently supports rust, zig, go, python, javascript, typescript, and vue buffers.')
    return
  endif

  const root = RunnerRootLabel(filetype)
  const actions = RunnerActions(filetype)
  const note = RunnerNote(filetype)

  echo '=========================================='
  echo 'Runner Information'
  echo '=========================================='
  echo '  Filetype:    ' .. filetype
  echo '  Buffer:      ' .. (empty(filepath) ? '[No Name]' : filepath)
  echo '  Mode:        ' .. RunnerModeLabel(filetype)
  echo '  Root:        ' .. (empty(root) ? 'n/a' : root)
  echo '  Actions:     ' .. join(actions, ', ')
  if !empty(note)
    echo '  Note:        ' .. note
  endif
  echo '=========================================='
enddef

def g:RunnerRunAction(action: string)
  const filetype = &filetype

  if action ==# 'repeat_last'
    if empty(last_run)
      g:ErrInfo('No previous runner command to repeat yet.')
      return
    endif
    ExecuteRunnerSpec(last_run, false)
    return
  endif

  if !SupportedFiletype(filetype)
    g:ErrWarn('Runner currently supports rust, zig, go, python, javascript, typescript, and vue buffers.')
    return
  endif

  if !EnsureRunnerReady(action)
    return
  endif

  ExecuteRunnerSpec(RunnerSpec(filetype, action))
enddef

def g:RunnerHealthCheck(): dict<any>
  return g:BuildManagedCommandModuleHealth('runner', 'Runner', config, 'RunnerInfo', {
    floaterm_available: g:CommandExists('FloatermNew'),
    workspace_available: exists('*g:WorkspaceProjectRoot'),
    supported_filetypes: config.supported_filetypes,
  })
enddef

command! -nargs=0 RunnerKeys call g:RunnerShowInfo()
command! -nargs=0 RunnerInfo call g:RunnerShowInfo()
command! -nargs=0 RunnerBuild call g:RunnerRunAction('build')
command! -nargs=0 RunnerRun call g:RunnerRunAction('run')
command! -nargs=0 RunnerTest call g:RunnerRunAction('test')
command! -nargs=0 RunnerTestFile call g:RunnerRunAction('test_file')
command! -nargs=0 RunnerRepeat call g:RunnerRunAction('repeat_last')
command! -nargs=0 RunnerClean call g:RunnerRunAction('clean')

# vim: set ft=vim sw=2 ts=2 sts=2 et:
