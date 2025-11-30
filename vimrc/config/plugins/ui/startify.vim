vim9script
# ============================================================================
# vim-startify 配置
# 启动界面插件
# ============================================================================

# 文件数量和基础设置
g:startify_files_number = 10
g:startify_fortune_use_unicode = 1

# 启动界面显示的项目（按照原来的顺序）
g:startify_lists = [
  {'type': 'commands',  'header': ['   Quick Actions']},
  {'type': 'files',     'header': ['   Recent Files']},
  {'type': 'bookmarks', 'header': ['   Bookmarks']},
  {'type': 'dir',       'header': ['   Project Directory']},
]

# 自定义 header（使用原来的自定义 ASCII 艺术字）
g:startify_custom_header = startify#center([
  " _      ____  _____ _     _  _     ",
  "/ \\__/|/   _\\/  __// \\ |\\/ \\/ \\__/|",
  "| |\\/|||  /  | |  _| | //| || |\\/||",
  "| |  |||  \\_ | |_//| \\\\// | || |  ||",
  "\\_/  \\|\\____/\\____\\\\__/  \\_/\\_/  \\|",
  "                                     ",
])

# 如果想用 fortune + cowsay，可以用：
# g:startify_custom_header = 'startify#center(startify#fortune#cowsay())'

# 自定义 footer（原来的简洁风格）
g:startify_custom_footer = startify#center([
  "",
  '  ⚡ Vim config by mcgeq',
])

# 快捷命令（使用 Clap，比 FZF 快）
g:startify_commands = [
  {'n': ['  New File', 'enew']},
  {'f': ['  Find File', 'Clap files']},
  {'o': ['  Recents', 'Clap history']},
  {'w': ['  Find Word', 'Clap grep']},
  {'b': ['  Buffers', 'Clap buffers']},
  {'s': ['  Last Session', 'SLoad']},
  {'c': ['  Config', $'edit {g:mcge_customvimrcdir}/init.vim']},
]

# 说明：
# - 使用 Clap 代替 FZF，启动更快，界面更现代
# - Clap 在 Windows 上性能优异，无需外部进程
# - 完整的 Clap 快捷键参见 config/mapping/clap_keys.vim

# 会话管理
g:startify_session_dir = g:mcge_customvimrcdir .. '/data/sessions'
g:startify_session_autoload = 1
g:startify_session_delete_buffers = 1
g:startify_session_persistence = 1

# 自动更新会话
g:startify_session_before_save = [
  'echo "Cleaning up before saving.."',
  'silent! NERDTreeTabsClose',
]

# 书签
g:startify_bookmarks = [
  {'v': g:mcge_customvimrcdir .. '/init.vim'},
  {'c': g:mcge_customvimrcdir .. '/local/user_settings.vim'},
]

# 改变目录到文件所在目录
g:startify_change_to_dir = 1

# 改变目录到 VCS 根目录
g:startify_change_to_vcs_root = 1

# 空白 buffer 也显示 startify
g:startify_enable_special = 0

# 相对路径
g:startify_relative_path = 1

# 自动更新
g:startify_update_oldfiles = 1

# vim: set ft=vim sw=2 ts=2 sts=2 et:
