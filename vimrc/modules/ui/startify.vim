vim9script
# ============================================================================
# 模块: UI / Startify
# 作者: mcge <mcgeq@outlook.com>
# 说明: 配置 Startify 启动页、会话和快捷入口。
# ============================================================================

# 防止重复加载
if g:MarkModuleLoaded('startify')
  finish
endif

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

def CenterStartifyLines(lines: list<string>): list<string>
  if g:FunctionExists('startify#center')
    return call('startify#center', [lines])
  endif

  return lines
enddef

# 初始化 Startify
def g:InitStartify(user_config: dict<any> = {})
  config = g:ResolveModuleConfig('startify', config, user_config)

  if g:ModuleIsDisabled(config, 'Startify')
    return
  endif

  g:ApplyGlobalVars({
    startify_files_number: config.files_number,
    startify_fortune_use_unicode: config.fortune_use_unicode,
    startify_lists: [
      {'type': 'commands',  'header': ['   Quick Actions']},
      {'type': 'files',     'header': ['   Recent Files']},
      {'type': 'bookmarks', 'header': ['   Bookmarks']},
      {'type': 'dir',       'header': ['   Project Directory']},
    ],
    startify_custom_header: CenterStartifyLines([
      " _      ____  _____ _     _  _     ",
      "/ \\__/|/   _\\/  __// \\ |\\/ \\/ \\__/|",
      "| |\\/|||  /  | |  _| | //| || |\\/||",
      "| |  |||  \\_ | |_//| \\\\// | || |  ||",
      "\\_/  \\|\\____/\\____\\\\__/  \\_/\\_/  \\|",
      "                                     ",
    ]),
    startify_custom_footer: CenterStartifyLines([
      "",
      '  ⚡ Vim config by mcgeq',
    ]),
    startify_commands: [
      {'n': ['  New File', 'enew']},
      {'f': ['  Find File', 'Clap files']},
      {'o': ['  Recents', 'Clap history']},
      {'w': ['  Find Word', 'Clap grep']},
      {'b': ['  Buffers', 'Clap buffers']},
      {'s': ['  Last Session', 'SLoad']},
      {'c': ['  Config', $"edit {g:mcge_customvimrcdir}/init.vim"]},
    ],
    startify_session_dir: config.session_dir,
    startify_session_autoload: config.session_autoload,
    startify_session_delete_buffers: config.session_delete_buffers,
    startify_session_persistence: config.session_persistence,
    startify_session_before_save: [
      'echo "Cleaning up before saving.."',
      'silent! NERDTreeTabsClose',
    ],
    startify_bookmarks: [
      {'v': g:mcge_customvimrcdir .. '/init.vim'},
      {'C': g:mcge_customvimrcdir .. '/local/user_settings.vim'},
    ],
    startify_change_to_dir: config.change_to_dir,
    startify_change_to_vcs_root: config.change_to_vcs_root,
    startify_enable_special: config.enable_special,
    startify_relative_path: config.relative_path,
    startify_update_oldfiles: config.update_oldfiles,
  })

  call g:ErrDebug('Startify initialized')
enddef

# 健康检查
def g:StartifyHealthCheck(): dict<any>
  var session_dir_exists = isdirectory(config.session_dir)

  return g:BuildManagedCommandModuleHealth('startify', 'Startify', config, 'Startify', {
    session_dir: config.session_dir,
    session_dir_exists: session_dir_exists,
    bookmarks_count: len(get(g:, 'startify_bookmarks', [])),
  })
enddef

# 获取配置
def g:GetStartifyConfig(): dict<any>
  return config
enddef

# 立即初始化
call g:InitStartify()

# vim: set ft=vim sw=2 ts=2 sts=2 et:
