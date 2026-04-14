vim9script

const SECTION_ORDER = [
  'Basic Editing',
  'Windows and Buffers',
  'LSP',
  'Explorer',
  'Search and Navigation',
  'Vista',
  'Git',
  'Text Objects',
  'Toggle',
  'Terminal',
  'UI',
  'Other',
]

const SECTION_TITLES = {
  'Basic Editing': '基础编辑',
  'Windows and Buffers': '窗口与 Buffer',
  'LSP': 'CoC 与 LSP',
  'Explorer': 'Coc Explorer',
  'Search and Navigation': '搜索与导航',
  'Vista': 'Vista 大纲',
  'Git': 'Git',
  'Text Objects': '文本对象',
  'Toggle': '开关与清理',
  'Terminal': '终端',
  'UI': '界面与提示',
  'Other': '其他',
}

def SectionWeight(section: string): number
  const idx = index(SECTION_ORDER, section)
  return idx >= 0 ? idx : len(SECTION_ORDER)
enddef

def ModeWeight(mode: string): number
  return get({
    '': 0,
    n: 1,
    i: 2,
    v: 3,
    x: 4,
    o: 5,
    t: 6,
    c: 7,
  }, mode, 99)
enddef

def ModeLabel(mode: string): string
  return empty(mode) ? 'map' : mode
enddef

def NormalizeKey(lhs: string): string
  return substitute(lhs, '<space>', '<Space>', 'g')
enddef

def Humanize(text: string): string
  var value = text
  value = substitute(value, '^:', '', '')
  value = substitute(value, '^<Plug>', '', '')
  value = substitute(value, '^\((.*)\)$', '\1', '')
  value = substitute(value, '[<>]', '', 'g')
  value = substitute(value, '[_-]', ' ', 'g')
  return value
enddef

def AggregateEntries(entries: list<dict<any>>): list<dict<any>>
  var by_key: dict<dict<any>> = {}

  for entry in entries
    const desc = !empty(entry.desc) ? entry.desc : Humanize(entry.rhs)
    var action = get(entry, 'action', entry.rhs)
    if exists('*g:NormalizeDocAction')
      action = g:NormalizeDocAction(action, desc)
    endif
    const key = entry.section .. "\t" .. entry.lhs .. "\t" .. action .. "\t" .. desc

    if !has_key(by_key, key)
      by_key[key] = {
        section: entry.section,
        lhs: NormalizeKey(entry.lhs),
        action: action,
        desc: desc,
        modes: [],
      }
    endif

    if index(by_key[key].modes, entry.mode) < 0
      add(by_key[key].modes, entry.mode)
    endif
  endfor

  var rows = values(by_key)
  for row in rows
    sort(row.modes, (a, b) => ModeWeight(a) - ModeWeight(b))
  endfor
  sort(rows, (a, b) => a.section ==# b.section
    ? (a.lhs ==# b.lhs ? (a.action < b.action ? -1 : a.action > b.action ? 1 : 0) : a.lhs < b.lhs ? -1 : 1)
    : SectionWeight(a.section) - SectionWeight(b.section))
  return rows
enddef

def BuildLines(entries: list<dict<any>>): list<string>
  const generated_on = strftime('%Y-%m-%d')
  const generate_cmd = 'vim -Nu vimrc/init.vim -i NONE -n -es -S vimrc/scripts/generate_keymaps_doc.vim -c "qall!"'
  var lines = [
    '# Vim 快捷键速查表',
    '',
    '> 自动生成，请勿手工编辑。',
    $'> 生成时间: {generated_on}',
    $'> 生成命令: `{generate_cmd}`',
    '',
    '## 目录',
    '',
  ]

  for section in SECTION_ORDER
    if empty(filter(copy(entries), (_, row) => row.section ==# section))
      continue
    endif
    const anchor = substitute(tolower(SECTION_TITLES[section]), '[^0-9a-z\u4e00-\u9fa5]+', '-', 'g')
    add(lines, $'- [{SECTION_TITLES[section]}](#{anchor})')
  endfor

  add(lines, '')
  add(lines, '---')
  add(lines, '')

  for section in SECTION_ORDER
    const section_rows = filter(copy(entries), (_, row) => row.section ==# section)
    if empty(section_rows)
      continue
    endif

    add(lines, $'## {SECTION_TITLES[section]}')
    add(lines, '')
    add(lines, '| 快捷键 | 模式 | 动作 | 说明 |')
    add(lines, '|--------|------|------|------|')
    for row in section_rows
      const mode_labels = map(copy(row.modes), (_, mode) => ModeLabel(mode))
      const mode_text = join(mode_labels, '/')
      add(lines, $'| `{row.lhs}` | `{mode_text}` | `{row.action}` | {row.desc} |')
    endfor
    add(lines, '')
  endfor

  while !empty(lines) && empty(lines[-1])
    remove(lines, -1)
  endwhile

  return lines
enddef

if !exists('*g:GetKeymapRegistry')
  throw 'g:GetKeymapRegistry() is unavailable'
endif

const keymap_rows = AggregateEntries(g:GetKeymapRegistry())
var doc_lines: list<any> = []
extend(doc_lines, BuildLines(keymap_rows))
writefile(doc_lines, 'vimrc/docs/keymaps.md')
