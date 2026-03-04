vim9script
# ============================================================================
# Terminal 模块 - 浮动终端
# 作者：mcge <mcgeq@outlook.com>
# 插件：vim-floaterm
# ============================================================================

# 防止重复加载
if exists('g:mcge_floaterm_loaded')
  finish
endif
g:mcge_floaterm_loaded = true

# 配置
var config = {
  enabled: true,
  shell: g:mcge_custom_shell,
  width: 0.8,
  height: 0.8,
  title: 'Floaterm',
  position: 'center',
  autofocus: 1,
  autoinsert: 1,
}

# 初始化 Floaterm
def g:InitFloaterm(user_config: dict<any> = {})
  # 合并用户配置
  extend(config, user_config)

  if !config.enabled
    call g:ErrDebug('Floaterm is disabled')
    return
  endif

  # 设置 shell
  g:floaterm_shell = config.shell

  # 窗口外观
  g:floaterm_width = config.width
  g:floaterm_height = config.height
  g:floaterm_title = config.title
  g:floaterm_position = config.position
  g:floaterm_autofocus = config.autofocus
  g:floaterm_autoinsert = config.autoinsert

  # 设置快捷键
  SetupMappings()

  call g:ErrDebug('Floaterm initialized')
enddef

# 设置快捷键
def SetupMappings()
  # F7: 新建终端
  nnoremap <silent> <F7> :FloatermNew<CR>
  tnoremap <silent> <F7> <C-\><C-n>:FloatermNew<CR>

  # F8: 上一个终端
  nnoremap <silent> <F8> :FloatermPrev<CR>
  tnoremap <silent> <F8> <C-\><C-n>:FloatermPrev<CR>

  # F9: 下一个终端
  nnoremap <silent> <F9> :FloatermNext<CR>
  tnoremap <silent> <F9> <C-\><C-n>:FloatermNext<CR>

  # F12: 切换终端
  nnoremap <silent> <F12> :FloatermToggle<CR>
  tnoremap <silent> <F12> <C-\><C-n>:FloatermToggle<CR>
enddef

# 健康检查
def g:FloatermHealthCheck(): dict<any>
  return {
    name: 'Floaterm',
    available: exists(':FloatermNew'),
    enabled: config.enabled,
    shell: config.shell,
    status: config.enabled ? 'running' : 'disabled',
  }
enddef

# 获取配置
def g:GetFloatermConfig(): dict<any>
  return config
enddef

# 立即初始化
call g:InitFloaterm()

# vim: set ft=vim sw=2 ts=2 sts=2 et:
