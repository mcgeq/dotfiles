vim9script
# ============================================================================
# UI 模块 - Startify 启动界面
# 作者：mcge <mcgeq@outlook.com>
# 插件：vim-startify
# ============================================================================

# 防止重复加载
if exists('g:mcge_startify_loaded')
  finish
endif
g:mcge_startify_loaded = true

# 配置
var config = {
  enabled: true,
  files_number: 10,
  fortune_use_unicode: 1,
  session_dir: g:mcge_customvimrcdir .. '/data/sessions',
  session_autoload: 1,
  session_delete_buffers: 1,
  session_persistence: 1,
  change_to_dir: 1,
  change_to_vcs_root: 1,
  enable_special: 0,
  relative_path: 1,
  update_oldfiles: 1,
}

# 初始化 Startify
def g:InitStartify(user_config: dict<any> = {})
  # 合并用户配置
  extend(config, user_config)

  if !config.enabled
    call g:ErrDebug('Startify is disabled')
    return
  endif

  # 基础设置
  g:startify_files_number = config.files_number
  g:startify_fortune_use_unicode = config.fortune_use_unicode

  # 启动界面显示的项目
  g:startify_lists = [
    {'type': 'commands',  'header': ['   Quick Actions']},
    {'type': 'files',     'header': ['   Recent Files']},
    {'type': 'bookmarks', 'header': ['   Bookmarks']},
    {'type': 'dir',       'header': ['   Project Directory']},
  ]

  # 自定义 header
  g:startify_custom_header = startify#center([
    " _      ____  _____ _     _  _     ",
    "/ \\__/|/   _\\/  __// \\ |\\/ \\/ \\__/|",
    "| |\\/|||  /  | |  _| | //| || |\\/||",
    "| |  |||  \\_ | |_//| \\\\// | || |  ||",
    "\\_/  \\|\\____/\\____\\\\__/  \\_/\\_/  \\|",
    "                                     ",
  ])

  # 自定义 footer
  g:startify_custom_footer = startify#center([
    "",
    '  ⚡ Vim config by mcgeq',
  ])

  # 快捷命令（使用 Clap）
  g:startify_commands = [
    {'n': ['  New File', 'enew']},
    {'f': ['  Find File', 'Clap files']},
    {'o': ['  Recents', 'Clap history']},
    {'w': ['  Find Word', 'Clap grep']},
    {'b': ['  Buffers', 'Clap buffers']},
    {'s': ['  Last Session', 'SLoad']},
    {'c': ['  Config', $"edit {g:mcge_customvimrcdir}/init.vim"]},
  ]

  # 会话管理
  g:startify_session_dir = config.session_dir
  g:startify_session_autoload = config.session_autoload
  g:startify_session_delete_buffers = config.session_delete_buffers
  g:startify_session_persistence = config.session_persistence

  # 自动更新会话
  g:startify_session_before_save = [
    'echo "Cleaning up before saving.."',
    'silent! NERDTreeTabsClose',
  ]

  # 书签
  g:startify_bookmarks = [
    {'v': g:mcge_customvimrcdir .. '/init.vim'},
    {'C': g:mcge_customvimrcdir .. '/local/user_settings.vim'},
  ]

  # 其他设置
  g:startify_change_to_dir = config.change_to_dir
  g:startify_change_to_vcs_root = config.change_to_vcs_root
  g:startify_enable_special = config.enable_special
  g:startify_relative_path = config.relative_path
  g:startify_update_oldfiles = config.update_oldfiles

  call g:ErrDebug('Startify initialized')
enddef

# 健康检查
def g:StartifyHealthCheck(): dict<any>
  var session_dir_exists = isdirectory(config.session_dir)

  return {
    name: 'Startify',
    available: exists(':Startify'),
    enabled: config.enabled,
    session_dir: config.session_dir,
    session_dir_exists: session_dir_exists,
    bookmarks_count: len(get(g:, 'startify_bookmarks', [])),
    status: config.enabled ? 'running' : 'disabled',
  }
enddef

# 获取配置
def g:GetStartifyConfig(): dict<any>
  return config
enddef

# 立即初始化
call g:InitStartify()

# vim: set ft=vim sw=2 ts=2 sts=2 et:
