vim9script

const normal_group_defaults = {modes: 'n'}
const lsp_cmd_defaults = {mode: 'n', silent: true, clear_cmdline: false, doc_section: 'LSP'}
const lsp_plug_defaults = {mode: 'n', silent: true, doc_section: 'LSP'}
const lsp_expr_defaults = {mode: 'i', silent: true, expr: true, doc_section: 'LSP'}
const search_cmd_defaults = {mode: 'n', silent: true, clear_cmdline: false, doc_section: 'Search and Navigation'}

const lsp_whichkey_groups = [
  {path: 'a', name: '+action', modes: ['n', 'v']},
  {path: 'e', name: '+explorer'},
  {path: 'r', name: '+refactor', modes: ['n', 'v']},
]
g:WhichKeyGroupMany(lsp_whichkey_groups, normal_group_defaults)

const explorer_maps = [
  {lhs: '<leader>ee', cmd: 'CocCommand explorer', desc: 'open'},
  {lhs: '<leader>ed', cmd: 'CocCommand explorer --preset .vim', desc: 'vim-files'},
  {lhs: '<leader>ef', cmd: 'CocCommand explorer --preset floating', desc: 'floating'},
  {lhs: '<leader>ec', cmd: 'CocCommand explorer --preset cocConfig', desc: 'config'},
  {lhs: '<leader>eb', cmd: 'CocCommand explorer --preset buffer', desc: 'buffers'},
  {lhs: '<leader>el', cmd: 'CocList explPresets', desc: 'presets'},
]
g:CmdMapMany(explorer_maps, {mode: 'n', remap: true, clear_cmdline: false, doc_section: 'Explorer'})

g:CmdMap(g:MapSpec({lhs: '<leader>y', cmd: 'CocList -A --normal yank', desc: 'yank-history'}, lsp_cmd_defaults))

const navigation_maps = [
  {lhs: '[g', plug: '(coc-diagnostic-prev)', desc: 'prev-diagnostic', doc_desc: 'previous diagnostic'},
  {lhs: ']g', plug: '(coc-diagnostic-next)', desc: 'next-diagnostic', doc_desc: 'next diagnostic'},
  {lhs: 'gd', plug: '(coc-definition)', desc: 'definition'},
  {lhs: 'gy', plug: '(coc-type-definition)', desc: 'type-definition', doc_desc: 'type definition'},
  {lhs: 'gi', plug: '(coc-implementation)', desc: 'implementation'},
  {lhs: 'gr', plug: '(coc-references)', desc: 'references'},
]
g:PlugMapMany(navigation_maps, lsp_plug_defaults)

const hover_maps = [
  {
    lhs: 'K',
    cmd: 'call CocActionAsync(''doHover'')',
    desc: 'hover',
  },
  {mode: 'i', lhs: '<C-k>', cmd: 'call CocActionAsync(''doHover'')', desc: 'hover'},
]
g:CmdMapMany(hover_maps, lsp_cmd_defaults)

const action_maps = [
  {mode: 'n', lhs: '<leader>rn', plug: '(coc-rename)', desc: 'rename'},
  {modes: ['x', 'n'], lhs: '<leader>af', plug: '(coc-format-selected)', desc: 'format'},
  {modes: ['x', 'n'], lhs: '<leader>aa', plug: '(coc-codeaction-selected)', desc: 'code-action', doc_desc: 'code action'},
  {mode: 'n', lhs: '<leader>ac', plug: '(coc-codeaction-cursor)', desc: 'cursor-action', doc_desc: 'code action at cursor'},
  {mode: 'n', lhs: '<leader>as', plug: '(coc-codeaction-source)', desc: 'source-action', doc_desc: 'source action'},
  {mode: 'n', lhs: '<leader>qf', plug: '(coc-fix-current)', desc: 'fix-current', doc_desc: 'fix current issue'},
  {mode: 'n', lhs: '<leader>re', plug: '(coc-codeaction-refactor)', silent: true, desc: 'refactor-menu', doc_desc: 'refactor menu'},
  {modes: ['x', 'n'], lhs: '<leader>ra', plug: '(coc-codeaction-refactor-selected)', silent: true, desc: 'selected-refactor', doc_desc: 'refactor selection'},
  {mode: 'n', lhs: '<leader>cl', plug: '(coc-codelens-action)', desc: 'codelens'},
]
g:PlugMapMany(action_maps, {doc_section: 'LSP'})

const text_object_maps = [
  {modes: ['x', 'o'], lhs: 'if', plug: '(coc-funcobj-i)', desc: 'inner-function', doc_desc: 'inner function'},
  {modes: ['x', 'o'], lhs: 'af', plug: '(coc-funcobj-a)', desc: 'around-function', doc_desc: 'around function'},
  {modes: ['x', 'o'], lhs: 'ic', plug: '(coc-classobj-i)', desc: 'inner-class', doc_desc: 'inner class'},
  {modes: ['x', 'o'], lhs: 'ac', plug: '(coc-classobj-a)', desc: 'around-class', doc_desc: 'around class'},
]
g:PlugMapMany(text_object_maps, {doc_section: 'Text Objects'})

if has('nvim-0.4.0') || has('patch-8.2.0750')
  const float_scroll_maps = [
    {modes: ['n', 'v'], lhs: '<M-f>', rhs: 'coc#float#has_scroll() ? coc#float#scroll(1) : "\<C-f>"', doc_action: 'coc#float#scroll(1)', desc: 'float-scroll-down'},
    {modes: ['n', 'v'], lhs: '<M-b>', rhs: 'coc#float#has_scroll() ? coc#float#scroll(0) : "\<C-b>"', doc_action: 'coc#float#scroll(0)', desc: 'float-scroll-up'},
    {mode: 'i', lhs: '<M-f>', rhs: 'coc#float#has_scroll() ? "\<C-r>=coc#float#scroll(1)\<CR>" : "\<Right>"', doc_action: 'coc#float#scroll(1)', desc: 'float-scroll-down'},
    {mode: 'i', lhs: '<M-b>', rhs: 'coc#float#has_scroll() ? "\<C-r>=coc#float#scroll(0)\<CR>" : "\<Left>"', doc_action: 'coc#float#scroll(0)', desc: 'float-scroll-up'},
  ]
  g:MapMany(float_scroll_maps, {silent: true, nowait: true, expr: true, doc_section: 'LSP'})
endif

const range_select_maps = [
  {modes: ['n', 'x'], lhs: '<M-s>', plug: '(coc-range-select)', desc: 'range-select', doc_desc: 'select range'},
]
g:PlugMapMany(range_select_maps, {silent: true, doc_section: 'LSP'})

const coc_list_maps = [
  {lhs: '<space>a', cmd: 'CocList diagnostics', desc: 'diagnostics'},
  {lhs: '<space>l', cmd: 'CocList extensions', desc: 'extensions'},
  {lhs: '<space>c', cmd: 'CocList commands', desc: 'commands'},
  {lhs: '<space>o', cmd: 'CocList outline', desc: 'outline'},
  {lhs: '<space>s', cmd: 'CocList -I symbols', desc: 'symbols'},
  {lhs: '<space>j', cmd: 'CocNext', desc: 'next'},
  {lhs: '<space>k', cmd: 'CocPrev', desc: 'prev'},
  {lhs: '<space>p', cmd: 'CocListResume', desc: 'resume'},
]
g:CmdMapMany(coc_list_maps, {mode: 'n', silent: true, nowait: true, clear_cmdline: false, doc_section: 'LSP'})

const completion_expr_maps = [
  {lhs: '<TAB>', rhs: 'coc#pum#visible() ? coc#pum#next(1) : g:CheckBackspace() ? "\<Tab>" : coc#refresh()', doc_action: 'coc#pum#next() / coc#refresh()', desc: 'completion-next', doc_desc: 'next completion item'},
  {lhs: '<CR>', rhs: 'coc#pum#visible() ? coc#pum#confirm() : "\<C-g>u\<CR>\<C-r>=coc#on_enter()\<CR>"', doc_action: 'coc#pum#confirm() / coc#on_enter()', desc: 'completion-confirm', doc_desc: 'confirm completion'},
]
g:MapMany(completion_expr_maps, lsp_expr_defaults)

g:Map(g:MapSpec({lhs: '<S-TAB>', rhs: 'coc#pum#visible() ? coc#pum#prev(1) : "\<C-h>"', doc_action: 'coc#pum#prev()', desc: 'completion-prev', doc_desc: 'previous completion item'}, lsp_expr_defaults))

if has('nvim')
  g:Map(g:MapSpec({lhs: '<C-Space>', rhs: 'coc#refresh()', doc_action: 'coc#refresh()', desc: 'trigger-completion', doc_desc: 'trigger completion'}, lsp_expr_defaults))
else
  g:Map(g:MapSpec({lhs: '<C-@>', rhs: 'coc#refresh()', doc_action: 'coc#refresh()', desc: 'trigger-completion', doc_desc: 'trigger completion'}, lsp_expr_defaults))
endif

const coc_search_maps = [
  {lhs: '<leader>ff', cmd: 'CocList files', desc: 'files'},
  {lhs: '<leader>fw', cmd: 'CocList grep', desc: 'grep'},
  {lhs: '<leader>fo', cmd: 'CocList outline', desc: 'outline'},
  {lhs: '<leader>fb', cmd: 'CocList buffers', desc: 'buffers'},
  {lhs: '<leader>fr', cmd: 'CocList mru', desc: 'recent'},
  {lhs: '<leader>fc', cmd: 'CocList cmdhistory', desc: 'command-history'},
  {lhs: '<leader>gf', cmd: 'CocList gfiles', desc: 'git-files'},
  {lhs: '<leader>fl', cmd: 'CocListResume', desc: 'resume'},
]
g:CmdMapMany(coc_search_maps, search_cmd_defaults)
