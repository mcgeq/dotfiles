vim9script
# ============================================================================
# CoC LSP 配置
# 作者: mcge <mcgeq@outlook.com>
# 说明: 统一管理 CoC 的配置、扩展和行为
# ============================================================================

# 默认配置
var config = {
  enabled: true,
  config_home: g:mcge_customvimrcdir .. "/config",
  data_home: g:mcge_customvimrcdir .. "/coc-data",
  auto_install: true,
  extensions: [
    # 语言服务
    "coc-clangd",              # C/C++
    "coc-pyright",             # Python
    "coc-rust-analyzer",       # Rust
    "coc-tsserver",            # TypeScript/JavaScript
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
    "coc-prettier",            # Prettier
    "@yaegassy/coc-tailwindcss3",  # TailwindCSS v3 (推荐，活跃维护)
    "coc-unocss",              # UnoCSS
    "coc-vetur",               # Vue 2
    "@yaegassy/coc-volar",     # Vue 3 (推荐，活跃维护)
    
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
    "coc-clang-format-style-options", # Clang Format
  ],
}

# 检查 CoC 是否可用
def IsCocAvailable(): bool
  return exists('g:did_coc_loaded')
enddef

# 初始化 CoC
def g:InitCoc(user_config: dict<any> = {})
  # 合并用户配置
  extend(config, user_config)
  
  if !config.enabled
    call g:ErrInfo('CoC is disabled')
    return
  endif
  
  # 设置路径
  g:coc_config_home = config.config_home
  g:coc_data_home = config.data_home
  
  # 设置扩展列表
  g:coc_global_extensions = config.extensions
  
  # 设置自动命令
  SetupAutocmds()
  
  # 设置自定义命令
  SetupCommands()
  
  # 集成状态栏
  set statusline^=%{coc#status()}%{get(b:,'coc_current_function','')}
  
  call g:ErrDebug('CoC LSP initialized')  # 改为调试级别
enddef

# 设置自动命令
def SetupAutocmds()
  augroup mcge_coc_lsp
    autocmd!
    
    # 高亮光标下的符号和引用
    autocmd CursorHold * silent call CocActionAsync('highlight')
    
    # 特定文件类型的格式化表达式
    autocmd FileType typescript,json setl formatexpr=CocAction('formatSelected')
    
    # 跳转占位符时更新签名帮助
    autocmd User CocJumpPlaceholder call CocActionAsync('showSignatureHelp')
  augroup END
enddef

# 设置自定义命令
def SetupCommands()
  # 格式化当前文件
  command! -nargs=0 Format call CocActionAsync('format')
  
  # 折叠代码
  command! -nargs=? Fold call CocAction('fold', <f-args>)
  
  # 组织导入
  command! -nargs=0 OR call CocActionAsync('runCommand', 'editor.action.organizeImport')
  
  # 重启 CoC
  command! -nargs=0 CocRestart call CocActionAsync('restart')
  
  # 显示 CoC 信息
  command! -nargs=0 CocInfo call ShowCocInfo()
enddef

# 显示 CoC 信息
def ShowCocInfo()
  echo '=========================================='
  echo 'CoC LSP Information'
  echo '=========================================='
  
  if IsCocAvailable()
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
  return {
    name: 'CoC LSP',
    available: IsCocAvailable(),
    enabled: config.enabled,
    extensions_count: len(config.extensions),
    status: IsCocAvailable() ? 'running' : 'not loaded',
  }
enddef

# 立即初始化（确保 Startify 快捷键可用）
call g:InitCoc()

# 如果需要延迟初始化（启动更快但 Startify 需要等待）：
# timer_start(200, (_) => {
#   call g:InitCoc()
# })

# vim: set ft=vim sw=2 ts=2 sts=2 et:
