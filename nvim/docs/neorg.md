# Neorg - 现代化笔记系统

## 📖 简介

Neorg 是专为 Neovim 设计的现代化笔记和组织系统，提供强大的文档编写、任务管理和知识组织功能。

## 🎯 核心功能

- ✅ 结构化笔记（标题、列表、代码块）
- ✅ GTD 任务管理（TODO、完成状态）
- ✅ 日程安排和提醒
- ✅ 链接系统（文件间跳转）
- ✅ 代码块语法高亮
- ✅ 导出功能（Markdown、HTML）
- ✅ 工作区管理

## 📝 基本语法

### 1. 标题层级

```norg
* 一级标题
** 二级标题
*** 三级标题
**** 四级标题
***** 五级标题
```

### 2. 列表

```norg
- 无序列表项 1
- 无序列表项 2
  -- 嵌套列表
  -- 嵌套列表

~ 任务列表
- ( ) 待办事项
- (x) 已完成
- (?) 需要信息
- (!) 重要
- (-) 已取消
```

### 3. 链接

```norg
{* 标题链接}
{:$/path/to/file:}          " 文件链接
{https://example.com}       " 网址链接
{:file.norg:* Heading}      " 文件中的标题
```

### 4. 代码块

```norg
@code lua
function hello()
  print("Hello from Neorg!")
end
@end

@code javascript
const greet = () => console.log("Hello");
@end
```

### 5. 强调和格式

```norg
*粗体*
/斜体/
_下划线_
-删除线-
!spoiler!
^上标^
,下标,
```

### 6. 引用

```norg
> 引用内容
> 可以多行
>> 嵌套引用
```

## ⌨️ 快捷键

### 基本操作

| 快捷键 | 功能 |
|--------|------|
| `<CR>` | 切换任务状态（TODO/DONE） |
| `<C-Space>` | 切换折叠 |
| `<C-t>` | 提升列表级别 |
| `<C-d>` | 降低列表级别 |
| `gO` | 打开目录 |
| `<C-]>` | 跳转到链接 |

### 模块操作

| 命令 | 功能 |
|------|------|
| `:Neorg index` | 打开索引 |
| `:Neorg workspace` | 切换工作区 |
| `:Neorg journal` | 打开日记 |
| `:Neorg toc` | 显示目录 |

## 💡 使用场景

### 1. 日常笔记

```norg
* 2024-11-30 学习笔记

** Neovim 配置
今天学习了 Neorg 的使用方法。

主要内容：
- 基本语法
- 任务管理
- 链接系统

** 待办事项
~ Tasks
- ( ) 完成项目文档
- ( ) 学习 Lua 编程
- (x) 配置 Neorg
```

### 2. 项目管理

```norg
* 项目：Web 应用开发

** 功能列表
~ Features
- (x) 用户登录
- (!) 数据导出 {重要}
- ( ) 邮件通知
- (?) API 文档 {需要确认格式}

** 技术栈
- Frontend: React + TypeScript
- Backend: Node.js + Express
- Database: PostgreSQL

** 相关链接
{:architecture.norg:* 系统架构}
{:api.norg:* API 文档}
```

### 3. GTD 工作流

```norg
* GTD 收集箱

~ Inbox
- ( ) 回复客户邮件
- ( ) 准备周会 PPT
- ( ) 修复 Bug #123

** 下一步行动
~ Next Actions
- (!) 完成季度报告 @work
- ( ) 买生日礼物 @personal
- ( ) 预约体检 @health

** 等待中
~ Waiting
- (?) 等待设计稿 @team
- (?) 等待服务器配置 @ops
```

### 4. 知识库

```norg
* 技术知识库

** 编程语言
- {:.javascript.norg:}
- {:.python.norg:}
- {:.rust.norg:}

** 框架和工具
- {:.react.norg:}
- {:.neovim.norg:}

** 最佳实践
- {:.clean-code.norg:}
- {:.design-patterns.norg:}
```

## 🗂️ 工作区管理

### 配置工作区

```lua
-- 在 Neorg 配置中
require("neorg").setup({
  load = {
    ["core.dirman"] = {
      config = {
        workspaces = {
          notes = "~/notes",
          work = "~/work-notes",
          personal = "~/personal",
        },
        default_workspace = "notes",
      },
    },
  },
})
```

### 使用工作区

```vim
:Neorg workspace notes     " 切换到 notes 工作区
:Neorg workspace work      " 切换到 work 工作区
:Neorg index               " 打开当前工作区索引
```

## 📅 日记功能

### 创建日记

```vim
:Neorg journal today       " 今天的日记
:Neorg journal yesterday   " 昨天的日记
:Neorg journal tomorrow    " 明天的日记
```

### 日记模板

```norg
* 2024-11-30 星期六

** 今日总结
- 完成了 Neorg 配置
- 学习了基本语法

** 明日计划
~ Tomorrow
- ( ) 继续学习 Neorg 高级功能
- ( ) 整理笔记系统

** 今日想法
学习新工具很有意思！
```

## 🎨 高级功能

### 1. 元数据

```norg
@document.meta
title: 我的笔记
description: Neorg 学习笔记
authors: mcgeq
categories: [learning, neovim]
created: 2024-11-30
updated: 2024-11-30
@end
```

### 2. 标签系统

```norg
* 项目 A #urgent #work
内容...

* 项目 B #personal #learning
内容...

" 可以按标签搜索和过滤
```

### 3. 数学公式

```norg
$inline math$
$$
display math
$$
```

## 🔗 导出功能

### 导出为 Markdown

```vim
:Neorg export to-file markdown output.md
```

### 支持的格式

- Markdown
- HTML
- LaTeX (部分)

## 💡 实用技巧

### 1. 快速创建模板

```norg
* 会议记录模板

** 基本信息
- 日期：
- 参与者：
- 主题：

** 讨论内容
-

** 行动项
~ Actions
- ( )
- ( )

** 下次会议
- 时间：
- 议题：
```

### 2. 使用目录

```vim
" 在文件开头
:Neorg toc

" 自动生成目录
* Table of Contents
** {* 第一章}
** {* 第二章}
** {* 第三章}
```

### 3. 任务优先级

```norg
~ Tasks by Priority
- (!) 高优先级 - 必须今天完成
- ( ) 中优先级 - 本周完成
- (-) 低优先级 - 可以延后
```

## 📊 与其他工具对比

| 特性 | Neorg | Org-mode | Markdown |
|------|-------|----------|----------|
| Neovim 原生 | ✅ | ❌ | ✅ |
| GTD 支持 | ✅ | ✅ | ❌ |
| 语法简洁 | ✅ | ⚠️ | ✅ |
| 功能丰富 | ✅ | ✅ | ❌ |
| 学习曲线 | 低 | 高 | 很低 |

## 🎯 最佳实践

### 1. 文件组织

```
~/notes/
├── index.norg          # 主索引
├── inbox.norg          # 收集箱
├── projects/
│   ├── project-a.norg
│   └── project-b.norg
├── areas/
│   ├── work.norg
│   └── personal.norg
└── journal/
    ├── 2024-11.norg
    └── 2024-12.norg
```

### 2. 定期整理

- 每天：清空收集箱
- 每周：回顾项目进度
- 每月：归档完成项目

### 3. 链接驱动

- 使用链接连接相关笔记
- 创建索引页面
- 建立知识图谱

## 🔧 配置建议

### 基础配置

```lua
{
  "nvim-neorg/neorg",
  opts = {
    load = {
      ["core.defaults"] = {},
      ["core.concealer"] = {},  -- 美化显示
      ["core.dirman"] = {
        config = {
          workspaces = {
            notes = "~/notes",
          },
        },
      },
      ["core.completion"] = {
        config = {
          engine = "nvim-cmp",
        },
      },
    },
  },
}
```

## 📚 总结

**Neorg 的优势**：
- 🚀 现代化设计
- 📝 强大的组织能力
- 🔗 灵活的链接系统
- ✅ GTD 工作流支持
- 💎 Neovim 原生体验

**适合场景**：
- 日常笔记
- 项目管理
- 知识管理
- GTD 实践
- 技术文档

开始用 `.norg` 文件记录你的想法和任务吧！🎉

## 🔗 相关资源

- [Neorg 官网](https://github.com/nvim-neorg/neorg)
- [Neorg Wiki](https://github.com/nvim-neorg/neorg/wiki)
- [示例工作区](https://github.com/nvim-neorg/neorg/wiki/Cookbook)
