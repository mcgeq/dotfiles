# Vim 快捷键速查表

> 基于 mcge vimrc 配置 | 最后更新: 2024-11-30

## 目录

- [基础操作](#基础操作)
- [窗口管理](#窗口管理)
- [Buffer 管理](#buffer-管理)
- [Coc 补全与 LSP](#coc-补全与-lsp)
- [Coc List 搜索](#coc-list-搜索)
- [Coc Explorer 文件浏览器](#coc-explorer-文件浏览器)
- [Vista 代码大纲](#vista-代码大纲)
- [Clap 模糊搜索](#clap-模糊搜索)
- [Git (Fugitive)](#git-fugitive)
- [GitGutter Git状态](#gitgutter-git状态)
- [Terminal (Floaterm)](#terminal-floaterm)
- [Matchup 匹配导航](#matchup-匹配导航)
- [Whitespace 空字符](#whitespace-空字符)

---

## 基础操作

| 快捷键 | 命令 | 模式 | 说明 |
|--------|------|------|------|
| `<C-x><C-s>` | :w | n/i/v | 保存文件 |
| `<C-x><C-q>` | :wq | n/i/v | 保存并退出 |
| `<C-a>` | 0 | n/i/v | 跳转到行首 |
| `<C-e>` | $ | n/i/v | 跳转到行尾 |
| `<C-h>` | \<Home\> | c | 命令行行首 |
| `<C-l>` | \<End\> | c | 命令行行尾 |
| `Q` | - | n | 禁用 EX 模式 |
| `q` | q | n | 录制宏 (覆盖原q功能) |

---

## 窗口管理

| 快捷键 | 命令 | 模式 | 说明 |
|--------|------|------|------|
| `<C-h>` | \<C-w\>h | n/t | 切换到左侧窗口 |
| `<C-j>` | \<C-w\>j | n/t | 切换到下方窗口 |
| `<C-k>` | \<C-w\>k | n/t | 切换到上方窗口 |
| `<C-l>` | \<C-w\>l | n/t | 切换到右侧窗口 |
| `q` | :close | n/v | 关闭窗口 |
| `<C-q>` | :bdelete | n/i/v | 删除当前 Buffer |

---

## Buffer 管理

| 快捷键 | 命令 | 模式 | 说明 |
|--------|------|------|------|
| `<M-q>` | :bdelete | n | 关闭当前 Buffer |
| `<C-p>` | :bprevious | n/i/v | 切换到上一个 Buffer |
| `<C-n>` | :bnext | n/i/v | 切换到下一个 Buffer |

---

## Coc 补全与 LSP

### 补全导航

| 快捷键 | 命令 | 模式 | 说明 |
|--------|------|------|------|
| `<Tab>` | coc#pum#next() | i | 确认补全 / 下一个候选 |
| `<S-TAB>` | coc#pum#prev() | i | 上一个候选 |
| `<CR>` | coc#pum#confirm() | i | 确认补全或格式化 |
| `<C-space>` | coc#refresh() | i | 触发补全 (nvim) |
| `<C-@>` | coc#refresh() | i | 触发补全 (vim) |

### 代码导航

| 快捷键 | 命令 | 模式 | 说明 |
|--------|------|------|------|
| `gd` | coc-definition | n | 跳转到定义 |
| `gy` | coc-type-definition | n | 跳转到类型定义 |
| `gi` | coc-implementation | n | 跳转到实现 |
| `gr` | coc-references | n | 查找引用 |
| `K` | CocAction('doHover') | n | 显示 Hover 文档 |
| `<C-k>` | CocAction('doHover') | i | 显示 Hover 文档 |

### 诊断跳转

| 快捷键 | 命令 | 模式 | 说明 |
|--------|------|------|------|
| `[g` | coc-diagnostic-prev | n | 跳转到上一个诊断 |
| `]g` | coc-diagnostic-next | n | 跳转到下一个诊断 |
| `<space>a` | CocList diagnostics | n | 显示所有诊断列表 |

### 代码操作

| 快捷键 | 命令 | 模式 | 说明 |
|--------|------|------|------|
| `<leader>rn` | coc-rename | n | 重命名符号 |
| `<leader>f` | coc-format-selected | n/v | 格式化选中代码 |
| `<leader>a` | coc-codeaction-selected | n/v | 选区代码操作 |
| `<leader>ac` | coc-codeaction-cursor | n | 光标处代码操作 |
| `<leader>as` | coc-codeaction-source | n | 当前缓冲区代码操作 |
| `<leader>qf` | coc-fix-current | n | 修复当前行诊断 |
| `<leader>re` | coc-codeaction-refactor | n | 代码重构 |
| `<leader>r` | coc-codeaction-refactor-selected | n/v | 选区重构 |

### 代码对象 (需 LSP 支持)

| 快捷键 | 命令 | 模式 | 说明 |
|--------|------|------|------|
| `if` | coc-funcobj-i | o/x | 函数内部 |
| `af` | coc-funcobj-a | o/x | 函数(包括) |
| `ic` | coc-classobj-i | o/x | 类内部 |
| `ac` | coc-classobj-a | o/x | 类(包括) |

### Coc List

| 快捷键 | 命令 | 模式 | 说明 |
|--------|------|------|------|
| `<space>a` | CocList diagnostics | n | 诊断列表 |
| `<space>l` | CocList extensions | n | 扩展列表 |
| `<space>c` | CocList commands | n | 命令列表 |
| `<space>o` | CocList outline | n | 当前文件大纲 |
| `<space>s` | CocList -I symbols | n | 工作区符号搜索 |
| `<space>j` | CocNext | n | 下一项 |
| `<space>k` | CocPrev | n | 上一项 |
| `<space>p` | CocListResume | n | 恢复上次列表 |

### Coc Yank

| 快捷键 | 命令 | 模式 | 说明 |
|--------|------|------|------|
| `<leader>y` | CocList yank | n | 打开剪贴板历史 |

### Float 窗口滚动

| 快捷键 | 命令 | 模式 | 说明 |
|--------|------|------|------|
| `<M-f>` | coc#float#scroll(1) | n/i/v | 向下滚动 Float 窗口 |
| `<M-b>` | coc#float#scroll(0) | n/i/v | 向上滚动 Float 窗口 |

### Range 选择

| 快捷键 | 命令 | 模式 | 说明 |
|--------|------|------|------|
| `<M-s>` | coc-range-select | n/v | 选择范围 (需 LSP 支持) |

---

## Coc List 搜索

| 快捷键 | 命令 | 说明 |
|--------|------|------|
| `<leader>ff` | CocList files | 文件搜索 |
| `<leader>fw` | CocList grep | 文本搜索 (ripgrep) |
| `<leader>fo` | CocList outline | 当前文件符号大纲 |
| `<leader>fb` | CocList buffers | 缓冲区列表 |
| `<leader>fr` | CocList mru | 最近文件 (MRU) |
| `<leader>fc` | CocList cmdhistory | 命令历史 |
| `<leader>fg` | CocList gfiles | Git 追踪的文件 |
| `<leader>fl` | CocListResume | 恢复上次的 CocList |

---

## Coc Explorer 文件浏览器

| 快捷键 | 命令 | 说明 |
|--------|------|------|
| `<leader>e` | CocCommand explorer | 打开 Explorer |
| `<leader>ed` | CocCommand explorer --preset .vim | 使用 .vim 预设打开 |
| `<leader>ef` | CocCommand explorer --preset floating | 使用浮动窗口打开 |
| `<leader>ec` | CocCommand explorer --preset cocConfig | 打开 CocConfig 目录 |
| `<leader>eb` | CocCommand explorer --preset buffer | Buffer 浏览器 |
| `<leader>el` | CocList explPresets | 列出所有预设 |

### Explorer 窗口内操作

| 按键 | 命令 | 说明 |
|------|------|------|
| `<tab>` | actionMenu | 打开操作菜单 |
| `e` | open | 用默认应用打开 |
| `s` | open:split | 水平分屏打开 |
| `E` | open:vsplit | 垂直分屏打开 |
| `t` | open:tab | 新标签页打开 |
| `h` | collapse | 折叠目录 |
| `l` | expand | 展开目录 |
| `<cr>` | cd, open | 进入目录/打开文件 |
| `<bs>` | gotoParent | 返回上级目录 |
| `a` | addFile | 新建文件 |
| `A` | addDirectory | 新建目录 |
| `r` | rename | 重命名 |
| `dd` | cutFile | 剪切 |
| `p` | pasteFile | 粘贴 |
| `df` | delete | 删除 |
| `zh` | toggleHidden | 显示/隐藏隐藏文件 |
| `R` | refresh | 刷新 |
| `?` | help | 帮助 |

---

## Vista 代码大纲

| 快捷键 | 命令 | 说明 |
|--------|------|------|
| `<F8>` | Vista!! | 打开/切换 Vista 大纲 |
| `<leader>v` | Vista!! | 打开/切换 Vista 大纲 |
| `<leader>vq` | Vista! | 强制关闭 Vista |
| `<leader>vf` | Vista finder | 模糊搜索符号 (当前文件) |
| `<leader>vF` | Vista finder! | 模糊搜索符号 (所有文件) |
| `<leader>vc` | Vista coc | 切换到 Coc 后端 |
| `<leader>vt` | Vista ctags | 切换到 ctags 后端 |
| `<leader>vl` | Vista vim_lsp | 切换到 vim-lsp 后端 |
| `<leader>vi` | Vista info | 显示 Vista 信息 |
| `<leader>vI` | Vista info+ | 显示详细信息 |

---

## Clap 模糊搜索

> 注意：Clap 使用 ripgrep 作为 grep 后端

### 文件和缓冲区

| 快捷键 | 命令 | 说明 |
|--------|------|------|
| `<leader>p` | Clap files | 搜索文件 |
| `<leader>P` | Clap gfiles | 搜索 Git 追踪的文件 |
| `<leader>bb` | Clap buffers | 搜索缓冲区 |
| `<leader>fh` | Clap history | 文件历史 |
| `<leader>op` | Clap | 打开 Clap 主界面 |
| `<leader>ob` | Clap buffers | Buffer 列表 |
| `<leader>of` | Clap files | 文件搜索 |
| `<leader>or` | Clap recent_files | 最近文件 |
| `<leader><Space>` | Clap files | 快速文件搜索 |

### 内容搜索

| 快捷键 | 命令 | 说明 |
|--------|------|------|
| `<leader>/` | Clap grep | 文本搜索 (ripgrep) |
| `<leader>fg` | Clap grep | 文本搜索 |
| `<leader>fl` | Clap blines | 当前文件行搜索 |
| `<leader>fL` | Clap lines | 所有打开文件行搜索 |
| `<leader>st` | Clap igrep | 智能 grep 搜索 (igrep) |
| `<leader>sg` | Clap grep | 普通 grep 搜索 |
| `<leader>sw` | Clap grep --query=\<cword\> | 搜索光标下单词 |
| `<leader>og` | Clap igrep | 智能 grep (igrep) |

### Git

| 快捷键 | 命令 | 说明 |
|--------|------|------|
| `<leader>gc` | Clap commits | Git 提交历史 |
| `<leader>gd` | Clap git_diff_files | Git 差异文件 |
| `<leader>gf` | Clap gfiles | Git 追踪的文件 |

### Vim 功能

| 快捷键 | 命令 | 说明 |
|--------|------|------|
| `<leader>:` | Clap command | 命令搜索 |
| `<leader>;` | Clap command_history | 命令历史 |
| `<leader>km` | Clap maps | 键位映射 |
| `<leader>?` | Clap help_tags | 帮助标签 |
| `<leader>tc` | Clap colors | 配色方案 |
| `<leader>oc` | Clap command | 命令列表 |
| `<leader>oh` | Clap history | 历史记录 |

### 导航

| 快捷键 | 命令 | 说明 |
|--------|------|------|
| `<leader>fm` | Clap marks | 标记 |
| `<leader>fj` | Clap jumps | 跳转列表 |
| `<leader>fr` | Clap registers | 寄存器 |
| `<leader>om` | Clap marks | 标记列表 |
| `<leader>ow` | Clap windows | 窗口列表 |
| `<leader>oj` | Clap jumps | 跳转列表 |
| `<leader>ot` | Clap tags | 标签列表 |

### 其他

| 快捷键 | 命令 | 说明 |
|--------|------|------|
| `<leader>oq` | Clap quickfix | Quickfix 列表 |
| `<leader>os` | Clap colors | 配色方案 |
| `<leader>df` | Clap filer | 文件浏览器 |

---

## Git (Fugitive)

| 快捷键 | 命令 | 说明 |
|--------|------|------|
| `<leader>gg` | :G | 打开 Git 界面 |
| `<leader>gb` | :Git blame | Git Blame |
| `<leader>gd` | :Gdiffsplit | Git 差异分屏 |
| `<leader>gw` | :Gwrite | Git 写入 (git add) |
| `<leader>gr` | :Gread | Git 读取 (git checkout) |
| `<leader>gm` | :Git commit | Git 提交 |
| `<leader>gc` | :Gclog | Git 提交历史 |
| `<leader>go` | :copen | 打开 quickfix 窗口 |
| `<leader>gq` | :cclose | 关闭 quickfix 窗口 |
| `<leader>gn` | :cnext | quickfix 下一项 |
| `<leader>gp` | :cprevious | quickfix 上一项 |

---

## GitGutter Git状态

| 快捷键 | 命令 | 说明 |
|--------|------|------|
| `]c` | GitGutterNextHunk | 跳转到下一个 Hunk |
| `[c` | GitGutterPrevHunk | 跳转到上一个 Hunk |
| `<leader>hs` | GitGutterStageHunk | Stage Hunk (暂存当前改动) |
| `<leader>hu` | GitGutterUndoHunk | Undo Hunk (撤销当前改动) |
| `<leader>hp` | GitGutterPreviewHunk | Preview Hunk (预览改动) |

### 符号说明

| 符号 | 说明 |
|------|------|
| `│` | 新增行 |
| `┌` | 修改行 |
| `╵` | 删除行 |
| `█` | 首行删除 |

---

## Terminal (Floaterm)

| 快捷键 | 命令 | 说明 |
|--------|------|------|
| `<F7>` | FloatermNew | 新建终端 |
| `<F8>` | FloatermPrev | 切换到上一个终端 |
| `<F9>` | FloatermNext | 切换到下一个终端 |
| `<F12>` | FloatermToggle | 显示/隐藏终端 |

---

## Matchup 匹配导航

| 快捷键 | 命令 | 说明 |
|--------|------|------|
| `g%` | matchup-g% | 跳转到匹配的括号 |
| `[%` | matchup-[% | 跳转到上一个匹配块 |
| `]%` | matchup-]% | 跳转到下一个匹配块 |
| `z%` | matchup-z% | 跳转到匹配的括号 (仅匹配) |
| `i%` | matchup-i% | 选择匹配块内部 (操作符) |
| `a%` | matchup-a% | 选择匹配块 (操作符) |

> Matchup 支持: `()` `[]` `{}` `if/endif` `while/endwhile` `for/endfor` `try/endtry` 等

---

## Whitespace 空字符

| 快捷键 | 命令 | 说明 |
|--------|------|------|
| `<leader>tw` | ToggleWhitespace | Toggle 显示尾随空格 |
| `<leader>ts` | StripWhitespace | Strip 删除当前文件尾随空格 |

> 默认会在保存时自动删除尾随空格 (除了 diff, gitcommit, markdown, xml)

---

## 快捷键速记技巧

| 前缀 | 功能 |
|------|------|
| `<leader>f` | Find/Format 相关 |
| `<leader>g` | Git 相关 |
| `<leader>v` | Vista/View 相关 |
| `<leader>c` | Coc 相关 |
| `<space>` | CocList 列表相关 |

---

## 自定义命令

| 命令 | 说明 |
|------|------|
| `:Format` | 格式化当前文件 |
| `:Fold` | 折叠代码 |
| `:OR` | 组织导入 |
| `:CocRestart` | 重启 CoC |
| `:CocInfo` | 显示 CoC 信息 |
