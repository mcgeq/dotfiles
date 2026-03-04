vim9script
# ============================================================================
# Clap 模块 - 现代化模糊搜索
# 作者：mcge <mcgeq@outlook.com>
# 官方文档：https://github.com/liuchengxu/vim-clap
# ============================================================================

# 防止重复加载
if exists('g:mcge_clap_loaded')
  finish
endif
g:mcge_clap_loaded = true

# 配置
var config = {
  enabled: true,
  theme: 'material_design_dark',
  enable_icon: 1,
  layout: {
    relative: 'editor',
    width: '85%',
    height: '50%',
    row: '15%',
    col: '7%',
  },
  enable_background_shadow: 1,
  prompt_format: '%provider_id%>>',
  enable_indicator: 1,
  preview_size: 10,
  preview_direction: 'UD',
  grep_delay: 200,
  grep_blink: [2, 100],
  cache_directory: '~/.cache/clap',
  provider_history_size: 50,
  insert_mode_only: 0,
  force_python: 0,
}

# 初始化 Clap
def g:InitClap(user_config: dict<any> = {})
  # 合并用户配置
  extend(config, user_config)

  if !config.enabled
    call g:ErrDebug('Clap is disabled')
    return
  endif

  # 外观设置
  g:clap_theme = config.theme
  g:clap_enable_icon = config.enable_icon
  g:clap_layout = config.layout
  g:clap_enable_background_shadow = config.enable_background_shadow
  g:clap_prompt_format = config.prompt_format
  g:clap_enable_indicator = config.enable_indicator

  # 预览和性能
  g:clap_preview_size = config.preview_size
  g:clap_preview_direction = config.preview_direction
  g:clap_provider_grep_delay = config.grep_delay
  g:clap_provider_grep_blink = config.grep_blink
  g:clap_cache_directory = expand(config.cache_directory)
  g:clap_provider_history_size = config.provider_history_size
  g:clap_insert_mode_only = config.insert_mode_only
  g:clap_force_python = config.force_python

  # 设置搜索工具
  SetupProviders()

  # 设置快捷键
  SetupMappings()

  call g:ErrDebug('Clap initialized')
enddef

# 设置 Provider
def SetupProviders()
  # 使用 ripgrep 和 fd（性能最佳）
  if executable('rg')
    g:clap_provider_grep_executable = 'rg'
    g:clap_provider_grep_opts = '--vimgrep --smart-case --hidden'
  endif

  if executable('fd')
    g:clap_provider_files_executable = 'fd'
    g:clap_provider_files_opts = '--type f --hidden --follow --exclude .git --exclude node_modules'
  endif
enddef

# 设置快捷键
def SetupMappings()
  # 基础搜索
  nnoremap <silent> <leader>p :Clap files<CR>
  nnoremap <silent> <leader>P :Clap gfiles<CR>
  nnoremap <silent> <leader><Space> :Clap files<CR>

  # 缓冲区
  nnoremap <silent> <leader>bb :Clap buffers<CR>

  # 最近文件
  nnoremap <silent> <leader>fh :Clap history<CR>

  # 文本搜索
  nnoremap <silent> <leader>/ :Clap grep<CR>
  nnoremap <silent> <leader>fg :Clap grep<CR>
  nnoremap <silent> <leader>st :Clap igrep<CR>
  nnoremap <silent> <leader>sg :Clap grep<CR>
  xnoremap <silent> <leader>sg :Clap grep --query=@visual<CR>
  nnoremap <silent> <leader>sw :Clap grep --query=<cword><CR>
  xnoremap <silent> <leader>sw :Clap grep --query=@visual<CR>

  # 行搜索
  nnoremap <silent> <leader>fl :Clap blines<CR>
  nnoremap <silent> <leader>fL :Clap lines<CR>

  # Git 相关
  nnoremap <silent> <leader>gc :Clap commits<CR>
  nnoremap <silent> <leader>gd :Clap git_diff_files<CR>
  nnoremap <silent> <leader>gf :Clap gfiles<CR>

  # Vim 功能
  nnoremap <silent> <leader>; :Clap command_history<CR>
  nnoremap <silent> <leader>km :Clap maps<CR>
  nnoremap <silent> <leader>? :Clap help_tags<CR>
  nnoremap <silent> <leader>tc :Clap colors<CR>

  # 导航
  nnoremap <silent> <leader>fm :Clap marks<CR>
  nnoremap <silent> <leader>fj :Clap jumps<CR>
  nnoremap <silent> <leader>fr :Clap registers<CR>
enddef

# 健康检查
def g:ClapHealthCheck(): dict<any>
  var rg_available = executable('rg')
  var fd_available = executable('fd')

  return {
    name: 'Clap',
    available: exists(':Clap'),
    enabled: config.enabled,
    theme: config.theme,
    rg_available: rg_available,
    fd_available: fd_available,
    status: config.enabled ? 'running' : 'disabled',
  }
enddef

# 获取配置
def g:GetClapConfig(): dict<any>
  return config
enddef

# 立即初始化
call g:InitClap()

# vim: set ft=vim sw=2 ts=2 sts=2 et:
