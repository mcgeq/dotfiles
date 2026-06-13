vim9script
# ============================================================================
# 组件: Bootstrap / Constants
# 作者: mcge <mcgeq@outlook.com>
# 说明: 全局常量、路径约定与运行时能力探测。
# ============================================================================

# ----------------------------------------------------------------------------
# 版本与路径
# ----------------------------------------------------------------------------
g:CONFIG_VERSION = '2.0.0'
g:CONFIG_NAME = 'mcge-vimrc'
g:MIN_VIM_VERSION = 900

g:VIMRC_ROOT = g:mcge_customvimrcdir
g:BOOTSTRAP_DIR = g:VIMRC_ROOT .. '/bootstrap'
g:CORE_DIR = g:VIMRC_ROOT .. '/core'
g:MODULES_DIR = g:VIMRC_ROOT .. '/modules'
g:CONFIG_DIR = g:VIMRC_ROOT .. '/config'
g:AFTER_DIR = g:VIMRC_ROOT .. '/after'
g:FTPLUGIN_DIR = g:AFTER_DIR .. '/ftplugin'
g:MAPPINGS_DIR = g:CONFIG_DIR .. '/mapping'
g:LOCAL_DIR = g:VIMRC_ROOT .. '/local'
g:DATA_DIR = g:VIMRC_ROOT .. '/data'
g:CACHE_DIR = g:VIMRC_ROOT .. '/cache'

# ----------------------------------------------------------------------------
# 平台与能力探测
# ----------------------------------------------------------------------------
g:IS_WINDOWS = has('win32') || has('win64')
g:IS_MAC = has('mac') || has('macunix')
g:IS_LINUX = has('unix') && !g:IS_MAC
g:HAS_NVIM = has('nvim')

g:HAS_GIT = executable('git')
g:HAS_RG = executable('rg')
g:HAS_FD = executable('fd')
g:HAS_NODE = executable('node')
g:HAS_PYTHON3 = has('python3')

# ----------------------------------------------------------------------------
# 特性与性能阈值
# ----------------------------------------------------------------------------
g:FEATURES = {
  lsp: g:HAS_PYTHON3 || g:HAS_NODE,
  fuzzy_finder: g:HAS_RG && g:HAS_FD,
  git: g:HAS_GIT,
  terminal: has('terminal') || g:HAS_NVIM,
}

g:PERFORMANCE = {
  startup_warning: 150,
  startup_error: 300,
  module_warning: 50,
  defer_time: 100,
}

# vim: set ft=vim sw=2 ts=2 sts=2 et:
