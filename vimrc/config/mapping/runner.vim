vim9script

const normal_group_defaults = {modes: 'n'}
const runner_cmd_defaults = {mode: 'n', silent: true, clear_cmdline: false, doc_section: 'Runner'}

const runner_whichkey_groups = [
  {path: 'r', name: '+runner'},
]
g:WhichKeyGroupMany(runner_whichkey_groups, normal_group_defaults)

const runner_maps = [
  {lhs: '<leader>ri', cmd: 'RunnerInfo', desc: 'info', doc_desc: 'show runner info for current buffer'},
  {lhs: '<leader>rb', cmd: 'RunnerBuild', desc: 'build', doc_desc: 'build current project or buffer target'},
  {lhs: '<leader>rr', cmd: 'RunnerRun', desc: 'run', doc_desc: 'run current project or buffer target'},
  {lhs: '<leader>rt', cmd: 'RunnerTest', desc: 'test', doc_desc: 'run current project or package tests'},
  {lhs: '<leader>rT', cmd: 'RunnerTestFile', desc: 'test-file', doc_desc: 'run tests for current file when supported'},
  {lhs: '<leader>rl', cmd: 'RunnerRepeat', desc: 'repeat', doc_desc: 'repeat last runner command'},
  {lhs: '<leader>rc', cmd: 'RunnerClean', desc: 'clean', doc_desc: 'clean current project or buffer target'},
]
g:CmdMapMany(runner_maps, runner_cmd_defaults)
