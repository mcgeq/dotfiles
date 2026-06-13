vim9script

const normal_group_defaults = {modes: 'n'}
const text_object_plug_defaults = {doc_section: 'Text Objects'}
const toggle_cmd_defaults = {mode: 'n', doc_section: 'Toggle'}

const editor_whichkey_groups = [
  {path: 't', name: '+toggle'},
]
g:WhichKeyGroupMany(editor_whichkey_groups, normal_group_defaults)

const matchup_maps = [
  {modes: ['o', 'x'], lhs: 'i', plug: '(matchup-i%)', desc: 'inner-match', doc_desc: 'inner match'},
  {modes: ['o', 'x'], lhs: 'a', plug: '(matchup-a%)', desc: 'around-match', doc_desc: 'around match'},
  {mode: 'n', lhs: 'g%', plug: '(matchup-g%)', desc: 'goto-match', doc_desc: 'jump to match'},
  {mode: 'n', lhs: '[%', plug: '(matchup-[%)', desc: 'prev-match', doc_desc: 'previous match'},
  {mode: 'n', lhs: ']%', plug: '(matchup-]%)', desc: 'next-match', doc_desc: 'next match'},
  {mode: 'n', lhs: 'z%', plug: '(matchup-z%)', desc: 'peek-match', doc_desc: 'peek match'},
]
g:PlugMapMany(matchup_maps, text_object_plug_defaults)

const whitespace_maps = [
  {lhs: '<leader>tw', cmd: 'ToggleWhitespace', desc: 'whitespace', doc_desc: 'toggle whitespace'},
  {lhs: '<leader>ts', cmd: 'StripWhitespace', desc: 'strip-whitespace', doc_desc: 'strip trailing whitespace'},
]
g:CmdMapMany(whitespace_maps, toggle_cmd_defaults)
