# Neorg 快速参考卡

## 自定义快捷键（<Leader> = Space）

| 快捷键 | 功能 |
|--------|------|
| `<Leader>nw` | 打开笔记工作区 |
| `<Leader>nt` | 今日日记 |
| `<Leader>ny` | 昨日日记 |
| `<Leader>nm` | 明日日记 |
| `<Leader>ni` | 工作区索引 |
| `<Leader>nr` | 关闭所有 norg 缓冲区 |
| `<Leader>nc` | 显示目录 |

## Snippet 快捷输入（输入后按 Tab）

### 标题
- `h1<Tab>` → `* `
- `h2<Tab>` → `** `
- `h3<Tab>` → `*** `
- `h4<Tab>` → `**** `

### 任务
- `todo<Tab>` → `- ( ) 待办事项`
- `done<Tab>` → `- (x) 已完成`
- `pending<Tab>` → `- (-) 进行中`
- `cancel<Tab>` → `- (_) 已取消`
- `urgent<Tab>` → `- (!) 紧急`

### 列表
- `ul<Tab>` → `- 列表项`
- `ol<Tab>` → `~ 列表项`

### 其他
- `link<Tab>` → 链接模板
- `code<Tab>` → 代码块
- `meta<Tab>` → 文档元数据
- `date<Tab>` → 当前日期
- `time<Tab>` → 当前时间
- `meeting<Tab>` → 会议记录模板
- `project<Tab>` → GTD 项目模板

## 默认快捷键（在 .norg 文件中）

### 任务管理
- `<C-Space>` - 切换 TODO 状态
- `<LocalLeader>td` - 标记完成

### 列表（Insert 模式）
- `<Alt-Enter>` - 新建列表项
- `<Tab>` - 增加缩进
- `<Shift-Tab>` - 减少缩进

### 导航
- `<LocalLeader>nn` - 下一个标题
- `<LocalLeader>np` - 上一个标题
- `<CR>` - 跟随链接
- `<C-o>` - 返回

### 折叠
- `za` - 切换折叠
- `zM` - 关闭所有
- `zR` - 打开所有

## 基本语法

```norg
* 标题
** 子标题

- ( ) 待办任务
- (x) 已完成

- 无序列表
~ 有序列表

*粗体* /斜体/ _下划线_ -删除线-

{:链接.norg:}
{https://url}[文字]

@code lua
代码
@end
```

## 快速开始

1. 打开工作区：`<Leader>nw`
2. 创建笔记：`:Neorg index`
3. 开始写作，使用 snippet 快速输入
4. 使用 `<C-Space>` 管理任务

完整手册：`:e ~/config/dotfiles/nvim/docs/neorg-guide.md`
