vim9script

const terminal_cmd_defaults = {modes: ['n', 't'], silent: true, clear_cmdline: false, doc_section: 'Terminal'}

const floaterm_maps = [
  {lhs: '<F7>', cmd: 'FloatermNew', desc: 'new-terminal', doc_desc: 'new terminal'},
  {lhs: '<F8>', cmd: 'FloatermPrev', desc: 'prev-terminal', doc_desc: 'previous terminal'},
  {lhs: '<F9>', cmd: 'FloatermNext', desc: 'next-terminal', doc_desc: 'next terminal'},
  {lhs: '<F12>', cmd: 'FloatermToggle', desc: 'toggle-terminal', doc_desc: 'toggle terminal'},
]
g:CmdMapMany(floaterm_maps, terminal_cmd_defaults)
