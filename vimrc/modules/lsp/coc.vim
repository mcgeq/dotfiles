vim9script
# ============================================================================
# 模块: LSP / CoC
# 作者: mcge <mcgeq@outlook.com>
# 说明: 配置 CoC 的扩展、命令和自动命令行为。
# ============================================================================

# 防止重复加载
if g:MarkModuleLoaded('coc')
  finish
endif

# 配置
var config = {
  enabled: true,
  config_home: g:mcge_customvimrcdir .. "/config",
  data_home: g:mcge_customvimrcdir .. "/coc-data",
  extensions: [
    # 语言服务
    "coc-clangd",              # C/C++
    "@yaegassy/coc-ty",        # Python (ty)
    "coc-rust-analyzer",       # Rust
    "coc-java",                # Java
    "coc-lua",                 # Lua
    "coc-vimlsp",              # VimScript
    "coc-zig",                 # Zig
    "coc-sh",                  # Shell
    "coc-clojure",             # Clojure
    
    # Web 开发
    "coc-html",                # HTML
    "coc-css",                 # CSS
    "coc-cssmodules",          # CSS Modules
    "coc-eslint",              # ESLint
    "coc-prettier",            # Prettier（保留配置入口，默认关闭）
    "@yaegassy/coc-tailwindcss3",  # TailwindCSS v3 (推荐，活跃维护)
    "coc-unocss",              # UnoCSS
    "@yaegassy/coc-volar",     # Vue 3 (推荐，Volar 语言服务器)
    
    # 数据格式
    "coc-json",                # JSON
    "coc-yaml",                # YAML
    "coc-xml",                 # XML
    "coc-toml",                # TOML
    
    # 工具和增强
    "coc-explorer",            # 文件浏览器
    "coc-git",                 # Git 集成
    "coc-highlight",           # 高亮
    "coc-lightbulb",           # 代码操作提示
    "coc-lists",               # 列表增强
    "coc-pairs",               # 自动配对
    "coc-snippets",            # 代码片段
    "coc-yank",                # 剪贴板历史
    "coc-tabnine",             # AI 补全
    "coc-webview",             # Webview 支持
    
    # 其他
    "coc-cmake",               # CMake
    "coc-sql",                 # SQL
    "coc-markdownlint",        # Markdown Lint
    "coc-markdown-preview-enhanced",  # Markdown 预览
    # 注意：coc-clang-format-style-options 已移除，clang-format 由 clangd 内置提供
  ],
}

# 检查 CoC 是否可用
def IsCocLoaded(): bool
  return exists('g:did_coc_loaded')
enddef

def g:CocHighlightCursorHold()
  if exists('*CocActionAsync')
    call('CocActionAsync', ['highlight'])
  endif
enddef

def g:CocShowJumpSignatureHelp()
  if exists('*CocActionAsync')
    call('CocActionAsync', ['showSignatureHelp'])
  endif
enddef

def g:CocFormatBuffer()
  if exists('*CocActionAsync')
    call('CocActionAsync', ['format'])
  endif
enddef

def g:CocOrganizeImports()
  if exists('*CocActionAsync')
    call('CocActionAsync', ['runCommand', 'editor.action.organizeImport'])
  endif
enddef

def g:CocRestartService()
  if exists('*CocActionAsync')
    call('CocActionAsync', ['restart'])
  endif
enddef

def g:CocFoldRange(...args: list<any>)
  if exists('*CocAction')
    call('CocAction', ['fold'] + args)
  endif
enddef

def g:CocFormatSelectedExpr(): number
  if exists('*CocAction')
    return call('CocAction', ['formatSelected'])
  endif

  return 0
enddef

def g:ApplyCocFormatExpr()
  &l:formatexpr = 'g:CocFormatSelectedExpr()'
enddef

def g:ConfigureVueCocBuffer()
  b:coc_root_patterns = [
    '.git',
    '.env',
    'package.json',
    'tsconfig.json',
    'jsconfig.json',
    'vite.config.ts',
    'vite.config.js',
    'vue.config.js',
    'nuxt.config.ts',
  ]
  setlocal iskeyword+=-
enddef

# 初始化 CoC
def g:InitCoc(user_config: dict<any> = {})
  config = g:ResolveModuleConfig('coc', config, user_config)

  if g:ModuleIsDisabled(config, 'CoC')
    return
  endif

  # 设置路径
  g:ApplyGlobalVars({
    coc_config_home: config.config_home,
    coc_data_home: config.data_home,
    coc_global_extensions: config.extensions,
  })

  # 设置自动命令
  RegisterCocAutocmds()

  # 设置自定义命令
  DefineCocCommands()

  # 集成状态栏
  set statusline^=%{coc#status()}%{get(b:,'coc_current_function','')}

  call g:ErrDebug('CoC LSP initialized')
enddef

# 设置自动命令
def RegisterCocAutocmds()
  augroup mcge_coc_lsp
    autocmd!

    # 高亮光标下的符号和引用
    autocmd CursorHold * silent call g:CocHighlightCursorHold()

    # 特定文件类型的格式化表达式
    autocmd FileType typescript,json call g:ApplyCocFormatExpr()

    # 跳转占位符时更新签名帮助
    autocmd User CocJumpPlaceholder call g:CocShowJumpSignatureHelp()

    # Vue 文件：设置 workspace root 模式
    autocmd FileType vue call g:ConfigureVueCocBuffer()
  augroup END
enddef

# 设置自定义命令
def DefineCocCommands()
  # 格式化当前文件
  command! -nargs=0 Format call g:CocFormatBuffer()
  
  # 折叠代码
  command! -nargs=? Fold call g:CocFoldRange(<f-args>)
  
  # 组织导入
  command! -nargs=0 OR call g:CocOrganizeImports()
  
  # 重启 CoC
  command! -nargs=0 CocRestart call g:CocRestartService()
  
  # 显示 CoC 信息
  command! -nargs=0 CocInfo call g:PrintCocInfo()
enddef

# 显示 CoC 信息
def g:PrintCocInfo()
  echo '=========================================='
  echo 'CoC LSP Information'
  echo '=========================================='
  
  if IsCocLoaded()
    echo '  Status: ✓ Running'
    echo $'  Extensions: {len(config.extensions)}'
    echo $'  Config home: {config.config_home}'
    echo $'  Data home: {config.data_home}'
  else
    echo '  Status: ✗ Not loaded'
  endif
  
  echo '=========================================='
enddef

# 获取配置
def g:GetCocConfig(): dict<any>
  return config
enddef

# 健康检查
def g:CocHealthCheck(): dict<any>
  return g:BuildManagedModuleHealth('coc', 'CoC LSP', config, {
    available: IsCocLoaded(),
    extensions_count: len(config.extensions),
    status: IsCocLoaded() ? 'running' : 'not loaded',
  })
enddef

# 立即初始化（确保 Startify 快捷键可用）
call g:InitCoc()

# 如果需要延迟初始化（启动更快但 Startify 需要等待）：
# timer_start(200, (_) => {
#   call g:InitCoc()
# })

# vim: set ft=vim sw=2 ts=2 sts=2 et:
