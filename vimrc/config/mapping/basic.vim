vim9script

const basic_cmd_defaults = {modes: ['n', 'i', 'v'], silent: true, clear_cmdline: false, doc_section: 'Basic Editing'}
const window_map_defaults = {doc_section: 'Windows and Buffers'}
const window_cmd_defaults = {clear_cmdline: false, doc_section: 'Windows and Buffers'}
const ui_cmd_defaults = {mode: 'n', silent: true, clear_cmdline: false, doc_section: 'UI'}

const save_and_quit_maps = [
  {lhs: '<C-x><C-s>', cmd: 'w', desc: 'save'},
  {lhs: '<C-x><C-q>', cmd: 'wq', desc: 'save-and-quit', doc_desc: 'save and quit'},
]
g:CmdMapMany(save_and_quit_maps, basic_cmd_defaults)

const window_switch_maps = [
  {lhs: '<c-h>', rhs: '<C-w>h', desc: 'window-left', doc_desc: 'focus left window'},
  {lhs: '<c-j>', rhs: '<C-w>j', desc: 'window-down', doc_desc: 'focus lower window'},
  {lhs: '<c-l>', rhs: '<C-w>l', desc: 'window-right', doc_desc: 'focus right window'},
]
g:MapMany(window_switch_maps, g:MapSpec({mode: ''}, window_map_defaults))

const terminal_window_maps = [
  {lhs: '<c-h>', rhs: '<c-\><c-n><c-w>h', desc: 'window-left', doc_desc: 'focus left window'},
  {lhs: '<c-j>', rhs: '<c-\><c-n><c-w>j', desc: 'window-down', doc_desc: 'focus lower window'},
  {lhs: '<c-l>', rhs: '<c-\><c-n><c-w>l', desc: 'window-right', doc_desc: 'focus right window'},
]
g:MapMany(terminal_window_maps, g:MapSpec({mode: 't'}, window_map_defaults))

const close_maps = [
  {lhs: 'q', cmd: 'close', desc: 'close-window', doc_desc: 'close window'},
]
g:CmdMapMany(close_maps, g:MapSpec({modes: ['n', 'v'], silent: true}, window_cmd_defaults))

const buffer_close_maps = [
  {lhs: '<C-q>', cmd: 'bdelete', desc: 'delete-buffer', doc_desc: 'close buffer'},
]
g:CmdMapMany(buffer_close_maps, g:MapSpec({modes: ['n', 'i', 'v'], silent: true}, window_cmd_defaults))

const line_boundary_maps = [
  {lhs: '<C-a>', rhs_by_mode: {n: '0', i: '<Esc>0', v: '<Esc>0'}, desc: 'line-start', doc_desc: 'jump to line start'},
  {lhs: '<C-e>', rhs_by_mode: {n: '$', i: '<Esc>$', v: '<Esc>$'}, desc: 'line-end', doc_desc: 'jump to line end'},
]
g:MapMany(line_boundary_maps, {modes: ['n', 'i', 'v'], silent: true, doc_section: 'Basic Editing'})

const cmdline_maps = [
  {lhs: '<C-h>', rhs: '<Home>', desc: 'cmdline-home', doc_desc: 'command line start'},
  {lhs: '<C-l>', rhs: '<End>', desc: 'cmdline-end', doc_desc: 'command line end'},
]
g:MapMany(cmdline_maps, {mode: 'c', doc_section: 'Basic Editing'})

const buffer_cycle_maps = [
  {lhs: '<M-q>', cmd: 'bdelete', desc: 'delete-buffer', doc_desc: 'close buffer'},
  {lhs: '<C-tab>', cmd: 'bnext', desc: 'next-buffer', doc_desc: 'next buffer'},
  {lhs: '<C-s-tab>', cmd: 'bprevious', desc: 'prev-buffer', doc_desc: 'previous buffer'},
]
g:CmdMapMany(buffer_cycle_maps, g:MapSpec({mode: 'n'}, window_cmd_defaults))

const buffer_navigation_maps = [
  {lhs: '<C-p>', cmd: 'bprevious', desc: 'prev-buffer', doc_desc: 'previous buffer'},
  {lhs: '<C-n>', cmd: 'bnext', desc: 'next-buffer', doc_desc: 'next buffer'},
]
g:CmdMapMany(buffer_navigation_maps, g:MapSpec({modes: ['n', 'i', 'v']}, window_cmd_defaults))

g:Map({mode: 'n', remap: true, lhs: 'Q', rhs: '<nop>', doc: false})
g:Map({mode: '', lhs: 'Q', rhs: 'q', doc: false})

g:CmdMap(g:MapSpec({lhs: '<leader>rr', cmd: 'source $MYVIMRC', desc: 'reload-config', doc_desc: 'reload configuration'}, ui_cmd_defaults))
