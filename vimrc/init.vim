vim9script
# ============================================================================
# Vim 配置主入口（优化版）
# 作者: mcge <mcgeq@outlook.com>
# 版本: 2.1.0
# 最后修改: 2026-04-12
# ============================================================================

# ----------------------------------------------------------------------------
# 性能监控开始
# ----------------------------------------------------------------------------
if !exists('g:mcge_startup_time_start')
  g:mcge_startup_time_start = reltime()
endif

# ----------------------------------------------------------------------------
# 基础路径配置
# ----------------------------------------------------------------------------
if !exists('g:mcge_customvimrcdir')
  if has('win32') || has('win64')
    g:mcge_customvimrcdir = 'D:/config/dotfiles/vimrc'
  else
    g:mcge_customvimrcdir = expand('~/dotfiles/vimrc')
  endif
endif

const config_root = g:mcge_customvimrcdir

def EnsureRuntimepath(path: string, prepend: bool = false)
  const normalized_path = substitute(fnamemodify(path, ':p'), '\\', '/', 'g')
  const runtime_paths = mapnew(split(&runtimepath, ','), (_, entry) => substitute(fnamemodify(entry, ':p'), '\\', '/', 'g'))
  if index(runtime_paths, normalized_path) >= 0
    return
  endif

  if prepend
    &runtimepath = path .. ',' .. &runtimepath
  else
    &runtimepath ..= ',' .. path
  endif
enddef

def SourceIfReadable(filepath: string): bool
  if !filereadable(filepath)
    return false
  endif

  execute 'source ' .. fnameescape(filepath)
  return true
enddef

def SourceFilesIfReadable(filepaths: list<string>)
  for filepath in filepaths
    SourceIfReadable(filepath)
  endfor
enddef

EnsureRuntimepath(config_root, true)
EnsureRuntimepath(config_root .. '/after')

# ----------------------------------------------------------------------------
# Bootstrap - 基础环境初始化
# ----------------------------------------------------------------------------
const bootstrap_dir = config_root .. '/bootstrap'

SourceIfReadable(bootstrap_dir .. '/constants.vim')

if SourceIfReadable(bootstrap_dir .. '/environment.vim')
  g:EnvironmentInitialize()
endif

SourceFilesIfReadable([
  bootstrap_dir .. '/settings.vim',
  config_root .. '/local/user_env.vim',
])

# ----------------------------------------------------------------------------
# Core - 核心功能模块
# ----------------------------------------------------------------------------
const core_dir = config_root .. '/core'

SourceFilesIfReadable([
  core_dir .. '/error_handler.vim',
  core_dir .. '/utils.vim',
  core_dir .. '/loader.vim',
  core_dir .. '/keymap.vim',
  core_dir .. '/module.vim',
  core_dir .. '/health.vim',
])

SourceFilesIfReadable([
  config_root .. '/local/module_overrides.vim',
])

def LoadPlannedFiles(plan: list<dict<any>>)
  for entry in plan
    const dir_path = entry.dir
    if !isdirectory(dir_path)
      continue
    endif

    var filepaths: list<string> = []
    if has_key(entry, 'files')
      filepaths = mapnew(copy(entry.files), (_, filename) => dir_path .. '/' .. filename)
    else
      filepaths = sort(g:GetAllVimFiles(dir_path))
    endif

    for filepath in filepaths
      g:LoadFile(filepath)
    endfor
  endfor
enddef

# ----------------------------------------------------------------------------
# Modules - 功能模块
# ----------------------------------------------------------------------------
const module_load_plan = [
  {
    dir: config_root .. '/modules/ui',
    files: ['appearance.vim', 'colorscheme.vim', 'airline.vim', 'startify.vim', 'whichkey.vim'],
  },
  {
    dir: config_root .. '/modules/lsp',
    files: ['coc.vim'],
  },
  {
    dir: config_root .. '/modules/git',
    files: ['gutter.vim'],
  },
  {
    dir: config_root .. '/modules/navigation',
    files: ['clap.vim', 'vista.vim'],
  },
  {
    dir: config_root .. '/modules/terminal',
    files: ['floaterm.vim'],
  },
  {
    dir: config_root .. '/modules/editor',
    files: [
      'tabsize.vim',
      'commenter.vim',
      'snippets.vim',
      'multi-cursor.vim',
      'match-pair.vim',
      'whitespace.vim',
      'tags.vim',
    ],
  },
]
LoadPlannedFiles(module_load_plan)

# ----------------------------------------------------------------------------
# Mappings - 按键映射
# ----------------------------------------------------------------------------
LoadPlannedFiles([
  {dir: config_root .. '/config/mapping'},
])

# ----------------------------------------------------------------------------
# Local - 用户自定义配置（可选）
# ----------------------------------------------------------------------------
const local_dir = config_root .. '/local'

SourceFilesIfReadable([
  local_dir .. '/user_settings.vim',
  local_dir .. '/user_mappings.vim',
])

# ----------------------------------------------------------------------------
# 保存文件时自动更新日期时间
# ----------------------------------------------------------------------------
augroup mcge_metadata
  autocmd!
  autocmd BufWritePre *.{rs,c,cpp,py,ts,cs} call g:AutoUpdateLastUpdateInfo()
augroup END

# ----------------------------------------------------------------------------
# 性能监控结束
# ----------------------------------------------------------------------------
var elapsed = reltimefloat(reltime(g:mcge_startup_time_start)) * 1000
g:mcge_startup_time = elapsed

if exists('g:mcge_debug_mode') && g:mcge_debug_mode
  echo $'Vim loaded in {printf("%.2f", elapsed)}ms'
  g:PrintReport()
endif

command! VimStartupTime echo $'Startup time: {printf("%.2f", g:mcge_startup_time)}ms'

timer_start(500, (_) => {
  var failed = g:GetFailedModules()
  if !empty(failed)
    g:ErrWarn($'{len(failed)} module(s) failed to load. Run :VimrcLoadReport for details.')
  endif
  
  if exists('*g:QuickHealthCheck')
    const issues = g:QuickHealthCheck()
    if !empty(issues)
      echohl WarningMsg
      echo 'Health check found issues. Run :CheckHealth for details.'
      echohl None
    endif
  endif
})

silent! doautocmd User McgeConfigLoaded

# vim: set ft=vim sw=2 ts=2 sts=2 et:
