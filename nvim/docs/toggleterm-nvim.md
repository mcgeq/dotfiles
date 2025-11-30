# toggleterm.nvim - 终端集成

## 📖 简介

`toggleterm.nvim` 在 Neovim 中提供强大的终端管理功能。支持浮动终端、多终端实例、方向切换等，并可集成 lazygit、htop 等 TUI 工具。

## 🎯 核心功能

- ✅ 浮动终端窗口
- ✅ 水平/垂直分割终端
- ✅ 多终端实例管理
- ✅ 持久化终端会话
- ✅ 集成 lazygit、node、python 等
- ✅ 快捷键快速切换

## ⌨️ 快捷键

### 基本操作

| 快捷键 | 功能 | 说明 |
|--------|------|------|
| `<C-\>` | 切换浮动终端 | 打开/关闭浮动终端 |
| `<leader>tf` | 打开浮动终端 | Float terminal |
| `<leader>th` | 打开水平终端 | Horizontal terminal |
| `<leader>tv` | 打开垂直终端 | Vertical terminal |
| `<leader>gg` | 打开 lazygit | Git TUI |
| `<leader>tn` | 打开 Node REPL | Node.js 交互式环境 |
| `<leader>tp` | 打开 Python REPL | Python 交互式环境 |

### 终端内操作

| 快捷键 | 功能 |
|--------|------|
| `<C-\><C-n>` | 退出终端模式（进入 Normal 模式）|
| `<C-\>` | 隐藏当前终端 |
| `<C-h/j/k/l>` | 在窗口间导航 |
| `i` 或 `a` | 重新进入终端模式 |

### 多终端管理

| 命令 | 功能 |
|------|------|
| `:ToggleTerm` | 切换终端 |
| `:1ToggleTerm` | 切换终端 1 |
| `:2ToggleTerm` | 切换终端 2 |
| `:ToggleTermSendCurrentLine` | 发送当前行到终端 |
| `:ToggleTermSendVisualLines` | 发送选中行到终端 |

## 🔥 使用场景

### 1. 快速运行命令

```bash
# 打开浮动终端
<C-\>

# 运行命令
npm run dev

# 隐藏终端（命令继续运行）
<C-\>

# 再次打开查看输出
<C-\>
```

### 2. 多终端工作流

```bash
# 终端 1: 运行开发服务器
:1ToggleTerm
npm run dev

# 终端 2: 运行测试
:2ToggleTerm
npm test

# 终端 3: Git 操作
:3ToggleTerm
git status

# 快速切换
:1ToggleTerm  # 查看开发服务器
:2ToggleTerm  # 查看测试结果
```

### 3. 集成 lazygit

```bash
# 方式一：快捷键
<leader>gg  # 打开 lazygit

# 方式二：命令
:LazyGit

# lazygit 使用
# j/k: 上下移动
# <Space>: 暂存/取消暂存
# c: 提交
# P: 推送
# q: 退出
```

### 4. REPL 开发

```javascript
// 在 JS 文件中
<leader>tn  // 打开 Node REPL

// 选择代码
vjj  // 选择几行

// 发送到 REPL 执行
:ToggleTermSendVisualLines
```

### 5. 分割终端布局

```bash
# 垂直分割（适合宽屏）
<leader>tv
npm run dev

# 水平分割（适合查看日志）
<leader>th
tail -f logs/app.log

# 浮动终端（快速操作）
<leader>tf
git status
```

## 💡 实用技巧

### 1. 持久化会话

```bash
# 终端会话在隐藏后依然运行
:1ToggleTerm
npm run dev  # 启动开发服务器
<C-\>        # 隐藏终端

# 继续编辑代码...
# 需要时重新打开
:1ToggleTerm  # 服务器仍在运行
```

### 2. 发送代码到终端

```python
# 在 Python 文件中
def add(a, b):
    return a + b

# 打开 Python REPL
<leader>tp

# 选择函数定义
vap  # 选择段落

# 发送到 REPL
:ToggleTermSendVisualSelection

# 在 REPL 中测试
add(2, 3)  # 输出：5
```

### 3. 快速编译运行

```cpp
// 在 C++ 文件中编辑
// main.cpp
#include <iostream>
int main() {
    std::cout << "Hello" << std::endl;
    return 0;
}

// 打开终端编译运行
<leader>tf
g++ main.cpp -o main && ./main
```

### 4. 监控日志

```bash
# 终端 1: 应用服务器
:1ToggleTerm direction=vertical
npm run dev

# 终端 2: 日志监控
:2ToggleTerm direction=horizontal
tail -f /var/log/app.log

# 布局：
# ┌─────────────┬──────────┐
# │             │          │
# │   Editor    │ Terminal │
# │             │   (1)    │
# ├─────────────┴──────────┤
# │      Terminal (2)      │
# └────────────────────────┘
```

### 5. Git 工作流

```bash
# 快速 Git 操作
<leader>gg  # 打开 lazygit

# lazygit 中的操作
# 1. 查看更改：j/k 移动
# 2. 暂存文件：<Space>
# 3. 提交：c -> 输入消息 -> <CR>
# 4. 推送：P
# 5. 查看历史：2 (切换到 commits 面板)
# 6. 退出：q
```

## 🎨 终端布局

### 浮动终端（推荐）

```
┌──────────────────────────────────┐
│                                  │
│         Editor Window            │
│   ┌──────────────────────┐       │
│   │  Floating Terminal   │       │
│   │  $ npm run dev       │       │
│   │  > Running...        │       │
│   └──────────────────────┘       │
│                                  │
└──────────────────────────────────┘
```

### 水平分割

```
┌──────────────────────────────────┐
│         Editor Window            │
│                                  │
├──────────────────────────────────┤
│   Terminal Window                │
│   $ npm test                     │
└──────────────────────────────────┘
```

### 垂直分割

```
┌─────────────────┬────────────────┐
│                 │                │
│  Editor Window  │   Terminal     │
│                 │   $ npm dev    │
│                 │                │
└─────────────────┴────────────────┘
```

## ⚙️ 高级配置

### 自定义终端

```lua
-- 创建自定义终端
local Terminal = require('toggleterm.terminal').Terminal

-- 自定义 lazygit
local lazygit = Terminal:new({
  cmd = "lazygit",
  hidden = true,
  direction = "float",
  float_opts = {
    border = "curved",
  },
})

function _lazygit_toggle()
  lazygit:toggle()
end

vim.keymap.set("n", "<leader>gg", _lazygit_toggle, { desc = "LazyGit" })
```

### 自定义窗口大小

```lua
{
  "akinsho/toggleterm.nvim",
  opts = {
    size = function(term)
      if term.direction == "horizontal" then
        return 15  -- 水平终端高度
      elseif term.direction == "vertical" then
        return vim.o.columns * 0.4  -- 垂直终端宽度（40%）
      end
    end,
  }
}
```

### 自定义浮动窗口

```lua
{
  "akinsho/toggleterm.nvim",
  opts = {
    float_opts = {
      border = "curved",  -- 'single' | 'double' | 'shadow' | 'curved'
      width = 120,
      height = 30,
      winblend = 3,
    }
  }
}
```

## 📚 集成工具

### 常用 TUI 工具

```bash
# Git 客户端
<leader>gg  # lazygit

# 系统监控
:ToggleTerm cmd="htop"

# 文件管理
:ToggleTerm cmd="ranger"

# 数据库客户端
:ToggleTerm cmd="mycli"

# HTTP 客户端
:ToggleTerm cmd="httpie"
```

### 语言 REPL

```lua
-- Node.js
<leader>tn

-- Python
<leader>tp

-- 其他语言
:ToggleTerm cmd="irb"      # Ruby
:ToggleTerm cmd="ghci"     # Haskell
:ToggleTerm cmd="clj"      # Clojure
```

## 🆚 对比内置终端

| 特性 | 内置 `:terminal` | toggleterm.nvim |
|------|-----------------|-----------------|
| 浮动窗口 | ❌ | ✅ |
| 快捷切换 | ❌ | ✅ |
| 多终端管理 | 手动 | 自动编号 |
| 持久化会话 | 需手动管理 | 自动管理 |
| 集成工具 | 需手动配置 | 开箱即用 |

## 🔗 相关资源

- [GitHub - toggleterm.nvim](https://github.com/akinsho/toggleterm.nvim)
- [AstroCommunity 插件页](https://github.com/AstroNvim/astrocommunity/tree/main/lua/astrocommunity/terminal-integration/toggleterm-nvim)
- [lazygit 官网](https://github.com/jesseduffield/lazygit)
