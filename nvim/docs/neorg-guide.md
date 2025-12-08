# Neorg 完整操作手册

## 目录

1. [基础语法](#基础语法)
2. [快捷键大全](#快捷键大全)
3. [工作区管理](#工作区管理)
4. [日记功能](#日记功能)
5. [实用技巧](#实用技巧)

---

## 基础语法

### 标题

```norg
* 一级标题
** 二级标题
*** 三级标题
**** 四级标题
```

### 列表

```norg
- 无序列表项
  -- 子项

~ 有序列表项
  ~~ 子项
```

### 待办事项

```norg
- ( ) 未开始
- (-) 进行中
- (x) 已完成
- (?) 需要信息
- (!) 紧急
- (_) 取消
```

### 文本格式

```norg
*粗体*
/斜体/
_下划线_
-删除线-
`代码`
```

### 链接

```norg
{* 标题名}           -- 链接到标题
{:文件名:}           -- 链接到文件
{https://url}[文字]  -- 外部链接
```

---

## 快捷键大全

### 待办事项 (Normal 模式)

- `<C-Space>` - 切换 TODO 状态
- `<LocalLeader>td` - 标记完成
- `<LocalLeader>tu` - 标记未完成
- `<LocalLeader>tp` - 标记进行中
- `<LocalLeader>tc` - 标记取消

### 列表操作 (Insert 模式)

- `<M-CR>` / `<Alt-Enter>` - 新建列表项
- `<C-t>` - 切换列表类型
- `<Tab>` - 增加缩进
- `<S-Tab>` - 减少缩进

### 标题导航 (Normal 模式)

- `<LocalLeader>nn` - 下一个标题
- `<LocalLeader>np` - 上一个标题

### 链接操作 (Normal 模式)

- `<CR>` - 跟随链接
- `<LocalLeader>lt` - 创建链接
- `<C-o>` - 返回

### 折叠操作 (Normal 模式)

- `za` - 切换折叠
- `zc` - 关闭折叠
- `zo` - 打开折叠
- `zM` - 关闭所有
- `zR` - 打开所有

---

## 工作区管理

### 命令

```vim
:Neorg workspace notes    " 切换工作区
:Neorg index             " 打开索引
:Neorg return            " 关闭所有 norg 缓冲区
```

### 配置示例

在 `neorg.lua` 中：

```lua
["core.dirman"] = {
  config = {
    workspaces = {
      notes = "~/notes",
      work = "~/work",
      personal = "~/personal",
    },
    default_workspace = "notes",
  },
},
```

---

## 日记功能

### 命令

```vim
:Neorg journal today      " 今天
:Neorg journal yesterday  " 昨天
:Neorg journal tomorrow   " 明天
:Neorg journal custom     " 自定义日期
```

### 模板

日记会自动创建，包含日期元数据。

---

## 实用技巧

### 1. 快速插入标题

**方法 1：直接输入**
```
* <Space>  " 一级标题
** <Space> " 二级标题
```

**方法 2：使用 Snippet**
```
h1<Tab>    " 展开为 * 
h2<Tab>    " 展开为 ** 
h3<Tab>    " 展开为 *** 
```

### 2. GTD 工作流

```norg
* 项目名称
- ( ) 任务 1
- ( ) 任务 2
  - ( ) 子任务 2.1
  - (x) 子任务 2.2
- (-) 任务 3 - 进行中
```

使用 `<C-Space>` 快速切换状态。

### 3. 知识管理

```norg
* 主题
  链接相关笔记：{:相关笔记1:}、{:相关笔记2:}
  
** 要点
   - 要点 1
   - 要点 2
```

### 4. 会议记录模板

```norg
@document.meta
title: 会议记录 - 2025-12-08
categories: [会议]
@end

* 会议信息
  - 时间：2025-12-08 10:00
  - 参与人：张三、李四
  
* 议题
  ** 议题 1
  - ( ) 待办事项 1
  - ( ) 待办事项 2
  
* 决定事项
  - 决定 1
  - 决定 2
```

### 5. 标签和搜索

使用标签：
```norg
#标签1 #标签2
```

全局搜索：`:Telescope live_grep`

---

## LocalLeader 设置

默认 LocalLeader 是 `,`。可在配置中修改：

```lua
vim.g.maplocalleader = "\\"  -- 改为反斜杠
```

---

## 常用命令速查

| 命令 | 功能 |
|------|------|
| `:Neorg` | 查看所有命令 |
| `:Neorg toc` | 显示目录 |
| `:Neorg keybind all` | 显示所有快捷键 |

---

更多信息：https://github.com/nvim-neorg/neorg
