vim9script

const normal_group_defaults = {modes: 'n'}
const git_insert_cmd_defaults = {modes: ['n', 'i'], silent: true, nowait: true, doc_section: 'Git'}
const git_normal_cmd_defaults = {mode: 'n', silent: true, nowait: true, doc_section: 'Git'}
const git_normal_plug_defaults = {mode: 'n', doc_section: 'Git'}

const git_whichkey_groups = [
  {path: 'g', name: '+git'},
  {path: 'h', name: '+hunk'},
]
g:WhichKeyGroupMany(git_whichkey_groups, normal_group_defaults)

const git_command_maps = [
  {lhs: '<leader>gg', cmd: 'G', clear_cmdline_by_mode: {n: false, i: true}, desc: 'fugitive'},
  {lhs: '<leader>gs', cmd: 'Git', desc: 'status'},
  {lhs: '<leader>gb', cmd: 'Git blame', desc: 'blame'},
  {lhs: '<leader>gd', cmd: 'Gdiffsplit', desc: 'diff'},
  {lhs: '<leader>gw', cmd: 'Gwrite', desc: 'write'},
  {lhs: '<leader>gR', cmd: 'Gread', desc: 'read'},
  {lhs: '<leader>gm', cmd: 'Git commit', desc: 'commit'},
  {lhs: '<leader>gc', cmd: 'Gclog', desc: 'commits-log', doc_desc: 'commit log'},
]
g:CmdMapMany(git_command_maps, git_insert_cmd_defaults)

const quickfix_maps = [
  {lhs: '<leader>go', cmd: 'copen', desc: 'open-quickfix', doc_desc: 'open quickfix'},
  {lhs: '<leader>gq', cmd: 'cclose', desc: 'close-quickfix', doc_desc: 'close quickfix'},
  {lhs: '<leader>gn', cmd: 'cnext', desc: 'next-quickfix', doc_desc: 'next quickfix item'},
  {lhs: '<leader>gp', cmd: 'cprevious', desc: 'prev-quickfix', doc_desc: 'previous quickfix item'},
]
g:CmdMapMany(quickfix_maps, git_normal_cmd_defaults)

const gutter_maps = [
  {lhs: ']c', plug: '(GitGutterNextHunk)', desc: 'next-hunk', doc_desc: 'next hunk'},
  {lhs: '[c', plug: '(GitGutterPrevHunk)', desc: 'prev-hunk', doc_desc: 'previous hunk'},
  {lhs: '<leader>hs', plug: '(GitGutterStageHunk)', desc: 'stage', doc_desc: 'stage hunk'},
  {lhs: '<leader>hu', plug: '(GitGutterUndoHunk)', desc: 'undo', doc_desc: 'undo hunk'},
  {lhs: '<leader>hp', plug: '(GitGutterPreviewHunk)', desc: 'preview', doc_desc: 'preview hunk'},
]
g:PlugMapMany(gutter_maps, git_normal_plug_defaults)
