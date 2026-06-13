vim9script

const ui_cmd_defaults = {modes: ['n', 'v'], silent: true, doc_section: 'UI'}
const normal_group_defaults = {modes: 'n'}

const ui_whichkey_groups = [
  {path: 'u', name: '+ui'},
]
g:WhichKeyGroupMany(ui_whichkey_groups, normal_group_defaults)

const whichkey_maps = [
  {
    lhs: '<leader>',
    cmd_by_mode: {
      n: 'WhichKey ''<Space>''',
      v: 'WhichKeyVisual ''<Space>''',
    },
    desc: 'show-which-key',
    doc_desc: 'show which-key menu',
  },
]
g:CmdMapMany(whichkey_maps, ui_cmd_defaults)
