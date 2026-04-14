# Vim 快捷键速查表

> 自动生成，请勿手工编辑。
> 生成时间: 2026-04-13
> 生成命令: `vim -Nu vimrc/init.vim -i NONE -n -es -S vimrc/scripts/generate_keymaps_doc.vim -c "qall!"`

## 目录

- [基础编辑](#基础编辑)
- [窗口与 Buffer](#窗口与 buffer)
- [CoC 与 LSP](#coc 与 lsp)
- [Coc Explorer](#coc explorer)
- [搜索与导航](#搜索与导航)
- [Vista 大纲](#vista 大纲)
- [Git](#git)
- [文本对象](#文本对象)
- [开关与清理](#开关与清理)
- [终端](#终端)
- [界面与提示](#界面与提示)

---

## 基础编辑

| 快捷键 | 模式 | 动作 | 说明 |
|--------|------|------|------|
| `<C-a>` | `n` | `0` | jump to line start |
| `<C-a>` | `i/v` | `<Esc>0` | jump to line start |
| `<C-e>` | `n` | `$` | jump to line end |
| `<C-e>` | `i/v` | `<Esc>$` | jump to line end |
| `<C-h>` | `c` | `<Home>` | command line start |
| `<C-l>` | `c` | `<End>` | command line end |
| `<C-x><C-q>` | `n/i/v` | `write and quit` | save and quit |
| `<C-x><C-s>` | `n/i/v` | `write` | save |

## 窗口与 Buffer

| 快捷键 | 模式 | 动作 | 说明 |
|--------|------|------|------|
| `<C-n>` | `n/i/v` | `next buffer` | next buffer |
| `<C-p>` | `n/i/v` | `previous buffer` | previous buffer |
| `<C-q>` | `n/i/v` | `delete buffer` | close buffer |
| `<C-s-tab>` | `n` | `previous buffer` | previous buffer |
| `<C-tab>` | `n` | `next buffer` | next buffer |
| `<M-q>` | `n` | `delete buffer` | close buffer |
| `<c-h>` | `map` | `<C-w>h` | focus left window |
| `<c-h>` | `t` | `<c-\><c-n><c-w>h` | focus left window |
| `<c-j>` | `map` | `<C-w>j` | focus lower window |
| `<c-j>` | `t` | `<c-\><c-n><c-w>j` | focus lower window |
| `<c-l>` | `map` | `<C-w>l` | focus right window |
| `<c-l>` | `t` | `<c-\><c-n><c-w>l` | focus right window |
| `q` | `n/v` | `close window` | close window |

## CoC 与 LSP

| 快捷键 | 模式 | 动作 | 说明 |
|--------|------|------|------|
| `<C-@>` | `i` | `coc#refresh()` | trigger completion |
| `<C-k>` | `i` | `hover` | hover |
| `<CR>` | `i` | `coc#pum#confirm() / coc#on_enter()` | confirm completion |
| `<M-b>` | `n/i/v` | `coc#float#scroll(0)` | float-scroll-up |
| `<M-f>` | `n/i/v` | `coc#float#scroll(1)` | float-scroll-down |
| `<M-s>` | `n/x` | `range select` | select range |
| `<S-TAB>` | `i` | `coc#pum#prev()` | previous completion item |
| `<Space>a` | `n` | `CoC list diagnostics` | diagnostics |
| `<Space>c` | `n` | `CoC list commands` | commands |
| `<Space>j` | `n` | `next CoC item` | next |
| `<Space>k` | `n` | `previous CoC item` | prev |
| `<Space>l` | `n` | `CoC list extensions` | extensions |
| `<Space>o` | `n` | `CoC list outline` | outline |
| `<Space>p` | `n` | `CoC list resume` | resume |
| `<Space>s` | `n` | `CoC list symbols` | symbols |
| `<TAB>` | `i` | `coc#pum#next() / coc#refresh()` | next completion item |
| `<leader>aa` | `n/x` | `code action` | code action |
| `<leader>ac` | `n` | `cursor action` | code action at cursor |
| `<leader>af` | `n/x` | `format` | format |
| `<leader>as` | `n` | `source action` | source action |
| `<leader>cl` | `n` | `code lens` | codelens |
| `<leader>qf` | `n` | `fix current` | fix current issue |
| `<leader>ra` | `n/x` | `selected refactor` | refactor selection |
| `<leader>re` | `n` | `refactor menu` | refactor menu |
| `<leader>rn` | `n` | `rename` | rename |
| `<leader>y` | `n` | `CoC list yank` | yank-history |
| `K` | `n` | `hover` | hover |
| `[g` | `n` | `previous diagnostic` | previous diagnostic |
| `]g` | `n` | `next diagnostic` | next diagnostic |
| `gd` | `n` | `definition` | definition |
| `gi` | `n` | `implementation` | implementation |
| `gr` | `n` | `references` | references |
| `gy` | `n` | `type definition` | type definition |

## Coc Explorer

| 快捷键 | 模式 | 动作 | 说明 |
|--------|------|------|------|
| `<leader>eb` | `n` | `CoC Explorer buffers` | buffers |
| `<leader>ec` | `n` | `CoC Explorer config` | config |
| `<leader>ed` | `n` | `CoC Explorer vim files` | vim-files |
| `<leader>ee` | `n` | `CoC Explorer` | open |
| `<leader>ef` | `n` | `CoC Explorer floating` | floating |
| `<leader>el` | `n` | `CoC list Explorer presets` | presets |

## 搜索与导航

| 快捷键 | 模式 | 动作 | 说明 |
|--------|------|------|------|
| `<leader>/` | `n` | `Clap grep` | grep |
| `<leader>;` | `n` | `Clap command history` | command history |
| `<leader><Space>` | `n` | `Clap files` | find files |
| `<leader>?` | `n` | `Clap help tags` | help tags |
| `<leader>P` | `n` | `Clap git files` | git files |
| `<leader>bb` | `n` | `Clap buffers` | buffers |
| `<leader>df` | `n` | `Clap filer` | filer |
| `<leader>fL` | `n` | `Clap lines` | lines |
| `<leader>fb` | `n` | `CoC list buffers` | buffers |
| `<leader>fc` | `n` | `CoC list command history` | command-history |
| `<leader>ff` | `n` | `CoC list files` | files |
| `<leader>fh` | `n` | `Clap history` | history |
| `<leader>fj` | `n` | `Clap jumps` | jumps |
| `<leader>fl` | `n` | `CoC list resume` | resume |
| `<leader>fm` | `n` | `Clap marks` | marks |
| `<leader>fo` | `n` | `CoC list outline` | outline |
| `<leader>fr` | `n` | `CoC list recent files` | recent |
| `<leader>fw` | `n` | `CoC list grep` | grep |
| `<leader>gf` | `n` | `CoC list git files` | git-files |
| `<leader>km` | `n` | `Clap maps` | maps |
| `<leader>ob` | `n` | `Clap buffers` | buffers |
| `<leader>oc` | `n` | `Clap command` | commands |
| `<leader>of` | `n` | `Clap files` | files |
| `<leader>og` | `n` | `Clap interactive grep` | grep |
| `<leader>oh` | `n` | `Clap history` | history |
| `<leader>oj` | `n` | `Clap jumps` | jumps |
| `<leader>om` | `n` | `Clap marks` | marks |
| `<leader>op` | `n` | `Clap` | open picker |
| `<leader>oq` | `n` | `Clap quickfix` | quickfix |
| `<leader>or` | `n` | `Clap recent files` | recent |
| `<leader>os` | `n` | `Clap colors` | colors |
| `<leader>ot` | `n` | `Clap tags` | tags |
| `<leader>ow` | `n` | `Clap windows` | windows |
| `<leader>p` | `n` | `Clap files` | find files |
| `<leader>sg` | `n` | `Clap grep` | grep |
| `<leader>sg` | `x` | `Clap grep` | grep selection |
| `<leader>st` | `n` | `Clap interactive grep` | interactive grep |
| `<leader>sw` | `x` | `Clap grep` | grep selection |
| `<leader>sw` | `n` | `Clap grep` | grep current word |
| `<leader>tc` | `n` | `Clap colors` | colors |

## Vista 大纲

| 快捷键 | 模式 | 动作 | 说明 |
|--------|------|------|------|
| `<leader>vF` | `n` | `Vista finder all scopes` | find symbol in all scopes |
| `<leader>vI` | `n` | `Vista detailed info` | show detailed info |
| `<leader>vb` | `n` | `Vista toggle` | show sidebar |
| `<leader>vc` | `n` | `Vista CoC backend` | use CoC backend |
| `<leader>ve` | `n` | `Vista focus` | focus outline |
| `<leader>vf` | `n` | `Vista finder` | find symbol |
| `<leader>vi` | `n` | `Vista info` | info |
| `<leader>vl` | `n` | `Vista vim-lsp backend` | use vim-lsp backend |
| `<leader>vq` | `n` | `Vista close` | close outline |
| `<leader>vr` | `n` | `Vista refresh` | refresh outline |
| `<leader>vt` | `n` | `Vista ctags backend` | use ctags backend |
| `<leader>vv` | `n` | `Vista toggle` | toggle outline |

## Git

| 快捷键 | 模式 | 动作 | 说明 |
|--------|------|------|------|
| `<leader>gR` | `n/i` | `Git read` | read |
| `<leader>gb` | `n/i` | `Git blame` | blame |
| `<leader>gc` | `n/i` | `Git commit log` | commit log |
| `<leader>gd` | `n/i` | `Git diff` | diff |
| `<leader>gg` | `n/i` | `Git status` | fugitive |
| `<leader>gm` | `n/i` | `Git commit` | commit |
| `<leader>gn` | `n` | `next quickfix item` | next quickfix item |
| `<leader>go` | `n` | `open quickfix` | open quickfix |
| `<leader>gp` | `n` | `previous quickfix item` | previous quickfix item |
| `<leader>gq` | `n` | `close quickfix` | close quickfix |
| `<leader>gs` | `n/i` | `Git status` | status |
| `<leader>gw` | `n/i` | `Git write` | write |
| `<leader>hp` | `n` | `preview` | preview hunk |
| `<leader>hs` | `n` | `stage` | stage hunk |
| `<leader>hu` | `n` | `undo` | undo hunk |
| `[c` | `n` | `previous hunk` | previous hunk |
| `]c` | `n` | `next hunk` | next hunk |

## 文本对象

| 快捷键 | 模式 | 动作 | 说明 |
|--------|------|------|------|
| `[%` | `n` | `previous match` | previous match |
| `]%` | `n` | `next match` | next match |
| `a` | `x/o` | `around match` | around match |
| `ac` | `x/o` | `around class` | around class |
| `af` | `x/o` | `around function` | around function |
| `g%` | `n` | `go to match` | jump to match |
| `i` | `x/o` | `inner match` | inner match |
| `ic` | `x/o` | `inner class` | inner class |
| `if` | `x/o` | `inner function` | inner function |
| `z%` | `n` | `peek match` | peek match |

## 开关与清理

| 快捷键 | 模式 | 动作 | 说明 |
|--------|------|------|------|
| `<leader>ts` | `n` | `whitespace cleanup` | strip trailing whitespace |
| `<leader>tw` | `n` | `whitespace toggle` | toggle whitespace |

## 终端

| 快捷键 | 模式 | 动作 | 说明 |
|--------|------|------|------|
| `<F12>` | `n/t` | `Floaterm toggle` | toggle terminal |
| `<F7>` | `n/t` | `Floaterm new` | new terminal |
| `<F8>` | `n/t` | `Floaterm previous` | previous terminal |
| `<F9>` | `n/t` | `Floaterm next` | next terminal |

## 界面与提示

| 快捷键 | 模式 | 动作 | 说明 |
|--------|------|------|------|
| `<leader>` | `n/v` | `Which-Key` | show which-key menu |
| `<leader>rr` | `n` | `reload vimrc` | reload configuration |
