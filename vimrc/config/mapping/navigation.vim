vim9script

const normal_group_defaults = {modes: 'n'}
const search_cmd_defaults = {mode: 'n', silent: true, doc_section: 'Search and Navigation'}
const search_nowait_cmd_defaults = {mode: 'n', silent: true, nowait: true, doc_section: 'Search and Navigation'}
const visual_search_cmd_defaults = {mode: 'x', silent: true, doc_section: 'Search and Navigation'}
const vista_cmd_defaults = {mode: 'n', silent: true, nowait: true, doc_section: 'Vista'}

const navigation_whichkey_groups = [
  {path: 'b', name: '+buffer'},
  {path: 'f', name: '+file'},
  {path: 'o', name: '+open'},
  {path: 'v', name: '+vista'},
]
g:WhichKeyGroupMany(navigation_whichkey_groups, normal_group_defaults)

const clap_picker_maps = [
  {lhs: '<leader>op', cmd: 'Clap', desc: 'providers', doc_desc: 'open picker'},
  {lhs: '<leader>ob', cmd: 'Clap buffers', desc: 'buffers'},
  {lhs: '<leader>oc', cmd: 'Clap command', desc: 'commands'},
  {lhs: '<leader>oh', cmd: 'Clap history', desc: 'history'},
  {lhs: '<leader>of', cmd: 'Clap files', desc: 'files'},
  {lhs: '<leader>df', cmd: 'Clap filer', desc: 'filer'},
  {lhs: '<leader>oq', cmd: 'Clap quickfix', desc: 'quickfix'},
  {lhs: '<leader>oj', cmd: 'Clap jumps', desc: 'jumps'},
  {lhs: '<leader>om', cmd: 'Clap marks', desc: 'marks'},
  {lhs: '<leader>ow', cmd: 'Clap windows', desc: 'windows'},
  {lhs: '<leader>ot', cmd: 'Clap tags', desc: 'tags'},
  {lhs: '<leader>os', cmd: 'Clap colors', desc: 'colors'},
  {lhs: '<leader>og', cmd: 'Clap igrep', desc: 'grep'},
  {lhs: '<leader>or', cmd: 'Clap recent_files', desc: 'recent'},
]
g:CmdMapMany(clap_picker_maps, search_nowait_cmd_defaults)

const clap_search_maps = [
  {lhs: '<leader>p', cmd: 'Clap files', desc: 'find-files', doc_desc: 'find files'},
  {lhs: '<leader>P', cmd: 'Clap gfiles', desc: 'git-files', doc_desc: 'git files'},
  {lhs: '<leader><Space>', cmd: 'Clap files', desc: 'find-files', doc_desc: 'find files'},
  {lhs: '<leader>bb', cmd: 'Clap buffers', desc: 'buffers'},
  {lhs: '<leader>fh', cmd: 'Clap history', desc: 'history'},
  {lhs: '<leader>/', cmd: 'Clap grep', desc: 'grep'},
  {lhs: '<leader>st', cmd: 'Clap igrep', desc: 'smart-grep', doc_desc: 'interactive grep'},
  {lhs: '<leader>sg', cmd: 'Clap grep', desc: 'grep'},
  {lhs: '<leader>sw', cmd: 'Clap grep --query=<cword>', desc: 'grep-cword', doc_desc: 'grep current word'},
  {lhs: '<leader>fL', cmd: 'Clap lines', desc: 'lines'},
  {lhs: '<leader>;', cmd: 'Clap command_history', desc: 'command-history', doc_desc: 'command history'},
  {lhs: '<leader>km', cmd: 'Clap maps', desc: 'maps'},
  {lhs: '<leader>?', cmd: 'Clap help_tags', desc: 'help-tags', doc_desc: 'help tags'},
  {lhs: '<leader>tc', cmd: 'Clap colors', desc: 'colors'},
  {lhs: '<leader>fm', cmd: 'Clap marks', desc: 'marks'},
  {lhs: '<leader>fj', cmd: 'Clap jumps', desc: 'jumps'},
]
g:CmdMapMany(clap_search_maps, search_cmd_defaults)

const clap_visual_maps = [
  {lhs: '<leader>sg', cmd: 'Clap grep --query=@visual', desc: 'grep-selection', doc_desc: 'grep selection'},
  {lhs: '<leader>sw', cmd: 'Clap grep --query=@visual', desc: 'grep-selection', doc_desc: 'grep selection'},
]
g:CmdMapMany(clap_visual_maps, visual_search_cmd_defaults)

const vista_maps = [
  {lhs: '<leader>vv', cmd: 'Vista!!', desc: 'toggle', doc_desc: 'toggle outline'},
  {lhs: '<leader>vb', cmd: 'Vista!!', desc: 'sidebar', doc_desc: 'show sidebar'},
  {lhs: '<leader>vq', cmd: 'Vista!', desc: 'close', doc_desc: 'close outline'},
  {lhs: '<leader>ve', cmd: 'Vista focus', desc: 'focus', doc_desc: 'focus outline'},
  {lhs: '<leader>vf', cmd: 'Vista finder', desc: 'finder', doc_desc: 'find symbol'},
  {lhs: '<leader>vF', cmd: 'Vista finder!', desc: 'finder-all', doc_desc: 'find symbol in all scopes'},
  {lhs: '<leader>vr', cmd: 'Vista refresh', desc: 'refresh', doc_desc: 'refresh outline'},
  {lhs: '<leader>vc', cmd: 'Vista coc', desc: 'backend-coc', doc_desc: 'use CoC backend'},
  {lhs: '<leader>vt', cmd: 'Vista ctags', desc: 'backend-ctags', doc_desc: 'use ctags backend'},
  {lhs: '<leader>vl', cmd: 'Vista vim_lsp', desc: 'backend-vim-lsp', doc_desc: 'use vim-lsp backend'},
  {lhs: '<leader>vi', cmd: 'Vista info', desc: 'info'},
  {lhs: '<leader>vI', cmd: 'Vista info+', desc: 'info-plus', doc_desc: 'show detailed info'},
]
g:CmdMapMany(vista_maps, vista_cmd_defaults)
