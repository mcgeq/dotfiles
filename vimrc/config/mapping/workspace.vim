vim9script

const normal_group_defaults = {modes: 'n'}
const project_cmd_defaults = {mode: 'n', silent: true, clear_cmdline: false, doc_section: 'Workspace'}
const session_cmd_defaults = {mode: 'n', silent: true, clear_cmdline: false, doc_section: 'Workspace'}
const terminal_cmd_defaults = {modes: ['n', 't'], silent: true, clear_cmdline: false, doc_section: 'Terminal'}

const workspace_whichkey_groups = [
  {path: 'P', name: '+project'},
  {path: 'm', name: '+session'},
  {path: 'l', name: '+terminal'},
]
g:WhichKeyGroupMany(workspace_whichkey_groups, normal_group_defaults)

const project_maps = [
  {lhs: '<leader>Pi', cmd: 'ProjectInfo', desc: 'info', doc_desc: 'show current project info'},
  {lhs: '<leader>Pr', cmd: 'ProjectRoot', desc: 'root', doc_desc: 'set cwd to current project root'},
  {lhs: '<leader>Pp', cmd: 'ProjectSelect', desc: 'select', doc_desc: 'select project'},
  {lhs: '<leader>PS', cmd: 'ProjectSessionSelect', desc: 'project-session', doc_desc: 'select project and load session'},
]
g:CmdMapMany(project_maps, project_cmd_defaults)

const session_maps = [
  {lhs: '<leader>mi', cmd: 'SessionInfo', desc: 'info', doc_desc: 'show current workspace session info'},
  {lhs: '<leader>ms', cmd: 'SessionSave', desc: 'save', doc_desc: 'save workspace session'},
  {lhs: '<leader>ml', cmd: 'SessionLoad', desc: 'load', doc_desc: 'load workspace session'},
  {lhs: '<leader>mS', cmd: 'SessionSelect', desc: 'select', doc_desc: 'select workspace session'},
  {lhs: '<leader>md', cmd: 'SessionDelete', desc: 'delete', doc_desc: 'delete workspace session'},
  {lhs: '<leader>mD', cmd: 'SessionSelectDelete', desc: 'select-delete', doc_desc: 'select workspace session to delete'},
]
g:CmdMapMany(session_maps, session_cmd_defaults)

const terminal_maps = [
  {lhs: '<leader>lt', cmd: 'TerminalCwd', desc: 'cwd-shell', doc_desc: 'open terminal in cwd'},
  {lhs: '<leader>lp', cmd: 'TerminalProject', desc: 'project-shell', doc_desc: 'open terminal in project root'},
  {lhs: '<leader>lb', cmd: 'TerminalBufferDir', desc: 'buffer-shell', doc_desc: 'open terminal in buffer directory'},
]
g:CmdMapMany(terminal_maps, terminal_cmd_defaults)
