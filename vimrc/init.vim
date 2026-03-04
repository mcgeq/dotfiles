vim9script
# ============================================================================
# Vim 配置主入口（优化版）
# 作者: mcge <mcgeq@outlook.com>
# 版本: 2.0.0
# 最后修改: 2026-03-04
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
execute $'set runtimepath+={config_root}'
execute $'set runtimepath+={config_root}/after'

# 设置 colors 和 ftplugin 路径
const mcge_color = config_root .. "/colors"
const mcge_ftplugin = config_root .. "/ftplugin"
execute $"set runtimepath+={mcge_color}"
execute $"set runtimepath+={mcge_ftplugin}"

# 设置 autoload 路径
const mcge_autoload = config_root .. "/config"
execute $"set runtimepath+={mcge_autoload}"

# ----------------------------------------------------------------------------
# Bootstrap - 基础环境初始化
# ----------------------------------------------------------------------------
const bootstrap_dir = config_root .. '/bootstrap'

# 加载常量定义
if filereadable(bootstrap_dir .. '/constants.vim')
  execute 'source ' .. bootstrap_dir .. '/constants.vim'
endif

# 环境检测与验证
if filereadable(bootstrap_dir .. '/environment.vim')
  execute 'source ' .. bootstrap_dir .. '/environment.vim'
  call g:EnvironmentInitialize()
endif

# 加载基础设置
if filereadable(bootstrap_dir .. '/settings.vim')
  execute 'source ' .. bootstrap_dir .. '/settings.vim'
endif

# 加载用户环境变量（从旧的 mcge-env.vim 迁移）
const user_env = config_root .. '/local/user_env.vim'
if filereadable(user_env)
  execute 'source ' .. user_env
endif

# ----------------------------------------------------------------------------
# Core - 核心功能模块
# ----------------------------------------------------------------------------
const core_dir = config_root .. '/core'

# 错误处理器（最先加载）
if filereadable(core_dir .. '/error_handler.vim')
  execute 'source ' .. core_dir .. '/error_handler.vim'
endif

# 工具函数库
if filereadable(core_dir .. '/utils.vim')
  execute 'source ' .. core_dir .. '/utils.vim'
endif

# 模块加载器
if filereadable(core_dir .. '/loader.vim')
  execute 'source ' .. core_dir .. '/loader.vim'
endif

# 健康检查系统
if filereadable(core_dir .. '/health.vim')
  execute 'source ' .. core_dir .. '/health.vim'
endif

# ----------------------------------------------------------------------------
# 加载现有配置（兼容层）
# ----------------------------------------------------------------------------

# 加载 core 配置
const config_core_dir = config_root .. '/config/core'
if isdirectory(config_core_dir)
  const core_files = sort(g:GetAllVimFiles(config_core_dir))
  for file in core_files
    call g:LoadFile(file)
  endfor
endif

# 加载 etc 配置
const config_etc_dir = config_root .. '/config/etc'
if isdirectory(config_etc_dir)
  const etc_files = sort(g:GetAllVimFiles(config_etc_dir))
  for file in etc_files
    # 跳过 mcge_base.vim（已在 bootstrap/settings.vim 中加载）
    if file !~# 'mcge_base\.vim$'
      call g:LoadFile(file)
    endif
  endfor
endif

# 加载 plugins 配置（跳过已迁移到 modules/ 的）
const config_plugins_dir = config_root .. '/config/plugins'
if isdirectory(config_plugins_dir)
  const plugin_files = sort(g:GetAllVimFiles(config_plugins_dir))
  for file in plugin_files
    # 跳过已迁移到 modules/ 的文件
    # 跳过：UI (ui.vim, airline.vim, startify.vim)、LSP (coc.vim)、导航 (clap.vim, vista.vim)、
    #      终端 (floaterm.vim)、编辑器 (edit/*.vim, tags.vim)
    if file !~# 'ui\.vim$' && file !~# 'airline\.vim$' && file !~# 'coc\.vim$' &&
         \ file !~# 'clap\.vim$' && file !~# 'vista\.vim$' &&
         \ file !~# 'floaterm\.vim$' &&
         \ file !~# 'nerdcommenter\.vim$' && file !~# 'tabsize\.vim$' && file !~# 'ultisnips\.vim$' &&
         \ file !~# 'visual-multi\.vim$' && file !~# 'matchup\.vim$' && file !~# 'whitespace\.vim$' &&
         \ file !~# 'startify\.vim$' && file !~# 'tags\.vim$'
      call g:LoadFile(file)
    endif
  endfor
endif

# ----------------------------------------------------------------------------
# Modules - 功能模块（懒加载）
# ----------------------------------------------------------------------------

# UI 模块 - 延迟加载
const ui_dir = config_root .. '/modules/ui'
if isdirectory(ui_dir)
  # 外观设置立即加载
  call g:LoadFile(ui_dir .. '/appearance.vim')

  # 配色方案立即加载
  call g:LoadFile(ui_dir .. '/colorscheme.vim')

  # 状态栏延迟加载（等待 airline 插件）
  call g:DeferLoad(ui_dir .. '/statusline.vim', 100)

  # Airline 状态栏配置
  call g:LoadFile(ui_dir .. '/airline.vim')

  # Startify 启动界面
  call g:LoadFile(ui_dir .. '/startify.vim')
endif

# LSP 模块 - 立即加载（确保 Startify 快捷键可用）
const lsp_dir = config_root .. '/modules/lsp'
if isdirectory(lsp_dir)
  # CoC LSP - 立即加载，确保 Startify 命令响应快速
  call g:LoadFile(lsp_dir .. '/coc.vim')
  # CoC 快捷键映射
  call g:LoadFile(lsp_dir .. '/mapping.vim')
  # 如果想延迟加载以加快启动：call g:DeferLoad(lsp_dir .. '/coc.vim', 50)
endif

# Git 模块
const git_dir = config_root .. '/modules/git'
if isdirectory(git_dir)
  call g:LoadFile(git_dir .. '/gutter.vim')
  call g:LoadFile(git_dir .. '/mapping.vim')
endif

# 导航模块
const nav_dir = config_root .. '/modules/navigation'
if isdirectory(nav_dir)
  call g:LoadFile(nav_dir .. '/clap.vim')
  call g:LoadFile(nav_dir .. '/vista.vim')
  call g:LoadFile(nav_dir .. '/mapping.vim')
endif

# 终端模块
const term_dir = config_root .. '/modules/terminal'
if isdirectory(term_dir)
  call g:LoadFile(term_dir .. '/floaterm.vim')
endif

# 编辑器增强模块
const editor_dir = config_root .. '/modules/editor'
if isdirectory(editor_dir)
  call g:LoadFile(editor_dir .. '/tabsize.vim')
  call g:LoadFile(editor_dir .. '/commenter.vim')
  call g:LoadFile(editor_dir .. '/snippets.vim')
  call g:LoadFile(editor_dir .. '/multi-cursor.vim')
  call g:LoadFile(editor_dir .. '/match-pair.vim')
  call g:LoadFile(editor_dir .. '/whitespace.vim')
  call g:LoadFile(editor_dir .. '/tags.vim')
endif

# ----------------------------------------------------------------------------
# Language - 语言特定配置（懒加载）
# ----------------------------------------------------------------------------
const lang_dir = config_root .. '/config/lang'
if isdirectory(lang_dir)
  augroup mcge_lang_loader
    autocmd!
    autocmd FileType python execute $'source {lang_dir}/python.vim'
    autocmd FileType rust execute $'source {lang_dir}/rust.vim'
    autocmd FileType typescript,javascript execute $'source {lang_dir}/typescript.vim'
    autocmd FileType zig execute $'source {lang_dir}/zig.vim'
    autocmd FileType html execute $'source {lang_dir}/html.vim'
    autocmd FileType css,scss execute $'source {lang_dir}/css.vim'
  augroup END
endif

# ----------------------------------------------------------------------------
# Mappings - 按键映射
# ----------------------------------------------------------------------------
const mappings_dir = config_root .. '/config/mapping'
if isdirectory(mappings_dir)
  const mapping_files = sort(g:GetAllVimFiles(mappings_dir))
  for file in mapping_files
    # 跳过已迁移到 modules/ 的映射文件
    # 跳过：git.vim, navigation.vim, clap_keys.vim, vista_keys.vim,
    #      coc.vim, coc_list.vim, terminal.vim
    if file !~# 'git\.vim$' && file !~# 'navigation\.vim$' &&
         \ file !~# 'clap_keys\.vim$' && file !~# 'vista_keys\.vim$' &&
         \ file !~# 'coc\.vim$' && file !~# 'coc_list\.vim$' &&
         \ file !~# 'terminal\.vim$'
      call g:LoadFile(file)
    endif
  endfor
endif

# ----------------------------------------------------------------------------
# Local - 用户自定义配置（可选）
# ----------------------------------------------------------------------------
const local_dir = config_root .. '/local'

if filereadable(local_dir .. '/user_settings.vim')
  execute 'source ' .. local_dir .. '/user_settings.vim'
endif

if filereadable(local_dir .. '/user_mappings.vim')
  execute 'source ' .. local_dir .. '/user_mappings.vim'
endif

# ----------------------------------------------------------------------------
# 保存文件时自动更新日期时间
# ----------------------------------------------------------------------------
autocmd BufWritePre *.{rs,c,cpp,py,ts,cs} call g:AutoUpdateLastUpdateInfo()

# ----------------------------------------------------------------------------
# 性能监控结束
# ----------------------------------------------------------------------------
var elapsed = reltimefloat(reltime(g:mcge_startup_time_start)) * 1000
g:mcge_startup_time = elapsed

# 如果启用调试模式，显示启动信息
if exists('g:mcge_debug_mode') && g:mcge_debug_mode
  echo $'Vim loaded in {printf("%.2f", elapsed)}ms'
  call g:PrintReport()
endif

# 定义性能查询命令
command! VimStartupTime echo $'Startup time: {printf("%.2f", g:mcge_startup_time)}ms'

# 启动后快速检查
timer_start(500, (_) => {
  var failed = g:GetFailedModules()
  if !empty(failed)
    call g:ErrWarn($'{len(failed)} module(s) failed to load. Run :VimrcLoadReport for details.')
  endif
  
  # 快速健康检查
  if exists('*g:QuickHealthCheck')
    const issues = g:QuickHealthCheck()
    if !empty(issues)
      echohl WarningMsg
      echo 'Health check found issues. Run :CheckHealth for details.'
      echohl None
    endif
  endif
})

# 触发自定义初始化完成事件（如果有监听器会响应）
silent! doautocmd User McgeConfigLoaded

# vim: set ft=vim sw=2 ts=2 sts=2 et:
