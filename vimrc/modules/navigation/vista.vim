vim9script
# ============================================================================
# Vista 模块 - 代码大纲和符号导航
# 作者：mcge <mcgeq@outlook.com>
# 官方文档：https://github.com/liuchengxu/vista.vim
# ============================================================================

# 防止重复加载
if exists('g:mcge_vista_loaded')
  finish
endif
g:mcge_vista_loaded = true

# 配置
var config = {
  enabled: true,
  default_executive: 'coc',
  sidebar_width: 35,
  icon_indent: ["▸ ", ""],
  highlight_whole_line: 1,
  enable_icon: 1,
  cursor_delay: 200,
  floating_preview: 1,
  floating_border: 'rounded',
  focus_on_icons: 1,
  update_on_text_changed: 0,
  update_on_text_changed_delay: 500,
  echo_cursor: 1,
  close_on_jump: 0,
  stay_on_open: 0,
  sidebar_position: 'vertical topleft',
}

# 图标映射
var icons = {
  'function': "\uf794",
  'variable': "\uf71b",
  'prototype': "\uf794",
  'macro': "\uf8a3",
  'class': "\uf0e8",
  'method': "\uf6a6",
  'struct': "\ufb44",
  'interface': "\ufa52",
  'enum': "\uf779",
  'field': "\uf93d",
  'constant': "\uf8ff",
  'module': "\uf668",
  'namespace': "\uf475",
  'package': "\ue612",
  'type': "\uf7fd",
  'typedef': "\uf7fd",
  'parameter': "\uf4a1",
  'default': "\uf29c",
}

# 初始化 Vista
def g:InitVista(user_config: dict<any> = {})
  # 合并用户配置
  extend(config, user_config)

  if !config.enabled
    call g:ErrDebug('Vista is disabled')
    return
  endif

  # Executive 配置
  g:vista_default_executive = config.default_executive
  g:vista_executive_for = {
    'vim': 'ctags',
    'python': 'coc',
    'javascript': 'coc',
    'typescript': 'coc',
    'rust': 'coc',
    'go': 'coc',
    'c': 'coc',
    'cpp': 'coc',
    'java': 'coc',
  }

  # 外观设置
  g:vista_sidebar_width = config.sidebar_width
  g:vista_icon_indent = config.icon_indent
  g:vista_highlight_whole_line = config.highlight_whole_line
  g:vista#renderer#enable_icon = config.enable_icon
  g:vista_cursor_delay = config.cursor_delay

  # 浮动窗口
  g:vista_floating_preview = config.floating_preview
  g:vista_floating_border = config.floating_border
  g:vista_focus_on_icons = config.focus_on_icons

  # 自动更新
  g:vista_update_on_text_changed = config.update_on_text_changed
  g:vista_update_on_text_changed_delay = config.update_on_text_changed_delay
  g:vista_echo_cursor = config.echo_cursor
  g:vista_close_on_jump = config.close_on_jump
  g:vista_stay_on_open = config.stay_on_open
  g:vista_sidebar_position = config.sidebar_position

  # 图标配置
  g:vista#renderer#icons = icons

  # Ctags 配置
  SetupCtags()

  # 设置快捷键
  SetupMappings()

  call g:ErrDebug('Vista initialized')
enddef

# 设置 Ctags
def SetupCtags()
  if executable('ctags')
    g:vista_ctags_cmd = {
      'haskell': 'hasktags -x -o - -c',
    }
    g:vista_ctags_options = '--fields=+niazS --extras=+q'
    g:vista_ctags_project_opts = '--recurse'
  endif
enddef

# 设置快捷键
def SetupMappings()
  # 基本操作
  nnoremap <silent> <F8> :Vista!!<CR>
  nnoremap <silent> <leader>v :Vista!!<CR>
  nnoremap <silent> <leader>vq :Vista!<CR>

  # 符号查找
  nnoremap <silent> <leader>vf :Vista finder<CR>
  nnoremap <silent> <leader>vF :Vista finder!<CR>

  # 后端切换
  nnoremap <silent> <leader>vc :Vista coc<CR>
  nnoremap <silent> <leader>vt :Vista ctags<CR>
  nnoremap <silent> <leader>vl :Vista vim_lsp<CR>

  # 信息查看
  nnoremap <silent> <leader>vi :Vista info<CR>
  nnoremap <silent> <leader>vI :Vista info+<CR>
enddef

# 健康检查
def g:VistaHealthCheck(): dict<any>
  var ctags_available = executable('ctags')
  var coc_available = exists('*CocAction')

  return {
    name: 'Vista',
    available: exists(':Vista'),
    enabled: config.enabled,
    default_executive: config.default_executive,
    ctags_available: ctags_available,
    coc_available: coc_available,
    icons_configured: true,
    status: config.enabled ? 'running' : 'disabled',
  }
enddef

# 获取配置
def g:GetVistaConfig(): dict<any>
  return config
enddef

# 立即初始化
call g:InitVista()

# vim: set ft=vim sw=2 ts=2 sts=2 et:
