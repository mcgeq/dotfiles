vim9script
# ============================================================================
# 全局常量定义
# 作者: mcge <mcgeq@outlook.com>
# 注意: 本文件通过 source 加载，不使用 export 关键字
# ============================================================================

# 版本信息
g:CONFIG_VERSION = '2.0.0'
g:CONFIG_NAME = 'mcge-vimrc'
g:MIN_VIM_VERSION = 900

# 路径常量
g:VIMRC_ROOT = g:mcge_customvimrcdir
g:BOOTSTRAP_DIR = g:VIMRC_ROOT .. '/bootstrap'
g:CORE_DIR = g:VIMRC_ROOT .. '/core'
g:MODULES_DIR = g:VIMRC_ROOT .. '/modules'
g:CONFIG_DIR = g:VIMRC_ROOT .. '/config'
g:LANG_DIR = g:VIMRC_ROOT .. '/config/lang'
g:MAPPINGS_DIR = g:VIMRC_ROOT .. '/config/mapping'
g:LOCAL_DIR = g:VIMRC_ROOT .. '/local'
g:DATA_DIR = g:VIMRC_ROOT .. '/data'
g:CACHE_DIR = g:VIMRC_ROOT .. '/cache'

# 系统检测
g:IS_WINDOWS = has('win32') || has('win64')
g:IS_MAC = has('mac') || has('macunix')
g:IS_LINUX = has('unix') && !g:IS_MAC
g:HAS_NVIM = has('nvim')

# 可执行文件检测
g:HAS_GIT = executable('git')
g:HAS_RG = executable('rg')
g:HAS_FD = executable('fd')
g:HAS_NODE = executable('node')
g:HAS_PYTHON3 = has('python3')

# 功能特性
g:FEATURES = {
  lsp: g:HAS_PYTHON3 || g:HAS_NODE,
  fuzzy_finder: g:HAS_RG && g:HAS_FD,
  git: g:HAS_GIT,
  terminal: has('terminal') || g:HAS_NVIM,
}

# 性能阈值（毫秒）
g:PERFORMANCE = {
  startup_warning: 150,
  startup_error: 300,
  module_warning: 50,
  defer_time: 100,
}

# vim: set ft=vim sw=2 ts=2 sts=2 et:
