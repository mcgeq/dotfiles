# Org-SuperTag 

> **⚠️ 重要提示（5.3.0 及以上版本）**
> 如果你是从 5.2.0 之前的版本升级，请在启用 `supertag-use-global-fields` 之前，**先完成数据库全局字段迁移**。
> 具体步骤见 `doc/GLOBAL-FIELD-MIGRATION-GUIDE_CN.md`。
>
> **🆕 新功能（5.3.0）：** 属性转字段迁移系统
> 将现有的 Org `:PROPERTIES:` 转换为结构化的数据库字段，实现更好的查询和自动化。
> 详见下方的"属性转字段迁移"部分。

[English](./README.md) | [中文](./README_CN.md)

## ⚡ 介绍

Org-SuperTag 是一个面向 Org 的“数据优先”工作流：把 `#tag` 当成表，把标题当成记录。

- **纯 Emacs Lisp**：无 Python/EPC，加载即用
- **结构化数据**：标题上的 `#tag` + 字段（可查询、可自动化）
- **本地数据库**：内存 store + 落盘持久化（读写快）
- **同步服务**：增量同步 + 安全护栏 + 诊断工具
- **视图与交互**：节点视图、表格视图、看板（更像“数据库视图”）
- **自动化**：字段变更触发规则，保持元数据一致
- **可选 Vault 隔离**：每个同步目录独立 DB/state（单活跃 Vault）

## 🚀 30 秒上手

配置笔记仓库，支持多笔记仓库——

```emacs-lisp
;; 安装
(straight-use-package '(org-supertag :host github :repo "yibie/org-supertag"))

;; 配置目录
;; 单 Vault（推荐）
(setq org-supertag-sync-directories '("~/org/"))

;; 多 Vault（每个目录独立 DB/state）
;; (setq org-supertag-sync-directories '("~/org/" "~/work-notes/"))
;; (setq org-supertag-sync-directories-mode 'vaults)

;; 初始化（仅一次）
M-x supertag-sync-full-initialize
```

## 🎯 核心概念

传统标签：`#tag`  
SuperTag：`#tag` + 结构化数据

```org
* 项目计划 #project
  - status: planning
  - priority: high
  - due: 2024-12-31
```

## 📋 常用命令

- `M-x supertag-add-tag` - 添加标签
- `M-x supertag-view-node` - 查看节点详情
- `M-x supertag-search` - 智能搜索
- `M-x supertag-capture` - 快速捕获
- `M-x supertag-view-kanban` - 看板视图

## 🔍 查询示例

```lisp
;; 高优先级项目
(supertag-search '(and (tag "project")
                       (field "priority" "high")))

;; 未读论文
(supertag-search '(and (tag "paper")
                       (field "status" "unread")))
```

## 🔄 4.x 迁移

1. `M-x load-file RET supertag-migration.el RET`
2. `M-x supertag-migrate-database-to-new-arch RET`
3. 重启 Emacs

## 🔄 属性转字段迁移

**将 Org 属性转换为结构化字段**

Org-SuperTag 5.3 引入了强大的迁移系统，可以将现有的 Org `:PROPERTIES:` 抽屉转换为结构化的数据库字段。这实现了更好的查询、自动化和数据管理。

### 两种迁移路径

#### 路径 1：已在使用 Org-roam（标题有 `:ID:` 属性）

如果您的 Org 文件标题已有 `:ID:` 属性：

1. **为所有标题添加 ID**（如果缺失）：
   ```elisp
   M-x supertag-migration-add-ids-to-org-headings
   ```
   选择您的 Org 目录，为所有标题添加 `:ID:` 属性。

2. **同步到数据库**：
   ```elisp
   M-x supertag-sync-full-rescan
   ```
   这会将所有节点及其属性导入数据库。

3. **转换属性为字段**：
   ```elisp
   M-x supertag-convert-properties-to-field
   ```
   选择一个属性（如 "LOCATION"），选择/创建标签，然后转换。

#### 路径 2：未使用 Org-roam（标题缺少 `:ID:` 属性）

如果您的 Org 文件标题没有 `:ID:` 属性：

1. **为所有标题添加 ID**：
   ```elisp
   M-x supertag-migration-add-ids-to-org-headings
   ```
   这会为所有标题添加 `:ID:` 属性。

2. **同步到数据库**：
   ```elisp
   M-x supertag-sync-full-rescan
   ```

3. **转换属性为字段**：
   ```elisp
   M-x supertag-convert-properties-to-field
   ```

### 转换过程中发生什么

当您转换像 `:LOCATION: London` 这样的属性时：

1. **数据库更新**：
   - 节点获得指定的标签（如 `#place`）
   - 字段值存储在数据库中（`LOCATION = "London"`）

2. **Org 文件更新**：
   - 标签插入标题：`* My Heading #place`
   - 属性抽屉保持不变以确保兼容性

3. **查询能力**：
   - 按字段查找：`(field "LOCATION" "London")`
   - 按标签查找：`(tag "place")`
   - 组合查询无缝工作

### 批量转换

一次转换多个属性：

```elisp
M-x supertag-batch-convert-properties-to-fields
```

这会显示每个属性的统计信息，让您为每个转换选择标签。

### 转换前后的示例

**转换前**：
```org
* 项目会议
:PROPERTIES:
:LOCATION: 会议室 A
:ATTENDEES: Alice, Bob, Carol
:END:
```

**转换后**（数据库中）：
- 节点有标签：`meeting`
- 字段：`LOCATION = "会议室 A"`，`ATTENDEES = "Alice, Bob, Carol"`

**Org 文件**：
```org
* 项目会议 #meeting
:PROPERTIES:
:LOCATION: 会议室 A
:ATTENDEES: Alice, Bob, Carol
:END:
```

### 优势

- **更好的查询**：按字段值搜索，而不仅仅是文本
- **自动化**：基于字段变化触发规则
- **数据完整性**：带验证的结构化数据
- **视图**：表格视图自动显示字段列
- **向后兼容**：原始属性保持完整

## ⚙️ 配置

```emacs-lisp
;; 单 Vault（推荐）
(setq org-supertag-sync-directories '("~/notes/"))

;; 多 Vault（每个目录独立 DB/state）
;; (setq org-supertag-sync-directories '("~/notes/" "~/work-notes/"))
;; (setq org-supertag-sync-directories-mode 'vaults)
```

说明：
- 默认不会随文件自动切换活跃 Vault（避免频繁 IO/重载）。需要时可设置 `(setq org-supertag-vault-auto-switch t)`。
- mode line 会显示 `ST[<vault>]` 提示当前文件属于哪个 Vault。
- 也可以手动执行 `M-x supertag-vault-activate` 切换。

智能扫描（同步安全）：
- 同步时会计算文件内容 hash，并与上次同步的 hash 对比。
- hash 未变化则跳过解析，数据库不会被改写。
- 这能防止仅时间戳变化或重复触发导致的数据库意外清空。

---

_为 Emacs 用户设计的 Notion 级知识管理_

## 🆚 和 org-roam / denote 的区别

| 工具 | 更擅长 | Org-SuperTag 的差异 |
| --- | --- | --- |
| org-roam | 双链/反向链接/知识图谱 | Org-SuperTag 更偏 **结构化与查询**：标签即表、字段、数据库查询与视图；同时也有很强的引用系统（`supertag-add-reference`），并可在节点/表格视图中透视引用关系。 |
| denote | 极简、文件为中心的笔记管理 | Org-SuperTag 增加 **数据库层**：字段、复杂查询与视图；denote 则刻意保持轻量与纯文件组织。 |
