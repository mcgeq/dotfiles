[English](README.md)

# Wezterm 配置

模块化、易维护的 Wezterm 配置，按逻辑模块分离关注点，便于扩展和维护。

## 目录结构

```
wezterm/
├── wezterm.lua              # 主入口
├── user.lua.example         # 个人覆盖模板（复制为 user.lua）
├── user.lua                 # 个人覆盖（已 gitignore，可选）
├── config/                   # 配置模块
│   ├── init.lua             # Config 类，提供 merge/append 工具
│   ├── constants.lua        # 集中管理常量（颜色、尺寸等）+ user.lua 合并
│   ├── appearance.lua       # 外观配置聚合器
│   │   └── appearance/      # 外观子模块
│   │       ├── window.lua
│   │       ├── background.lua
│   │       ├── tab_bar.lua
│   │       ├── cursor.lua
│   │       ├── scrollbar.lua
│   │       ├── performance.lua
│   │       └── colors.lua
│   ├── bindings.lua         # 按键配置聚合器
│   │   └── bindings/        # 按键子模块
│   │       ├── keys.lua     # 键盘快捷键（按功能组织）
│   │       ├── key_tables.lua
│   │       └── mouse.lua
│   ├── domains.lua          # SSH/WSL/Unix 域配置
│   ├── fonts.lua            # 字体配置
│   ├── general.lua          # 通用行为设置
│   └── launch.lua           # Shell 启动配置
├── events/                   # 事件处理
│   ├── right-status.lua     # 右侧状态栏
│   ├── tab-title.lua        # 标签页标题格式
│   └── new-tab-button.lua   # 新标签按钮行为
├── utils/                    # 工具函数
│   ├── platform.lua         # 平台检测
│   ├── shells.lua           # Shell 检测
│   └── math.lua             # 数学工具

```

## 设计原则

### 1. 模块化
- 每个配置方面拆分到独立模块
- 相关配置分组到子目录
- 易于定位和修改特定功能

### 2. 常量管理
- 所有魔法值（颜色、尺寸等）集中在 `config/constants.lua`
- 便于维护和更新外观设置
- 无硬编码值散落在各文件中

### 3. Config 类增强
`Config` 类提供：
- `append()`: 浅合并（用于非冲突选项）
- `merge()`: 深合并（用于嵌套配置如颜色）
- `get_options()`: 获取当前选项副本（用于调试）
- 更好的错误处理和重复检测

### 4. 功能组织
- 快捷键按功能组织（标签页、面板、字体等）
- 外观设置按组件分组（窗口、标签栏、光标等）
- 每个模块单一职责

## 快捷键

修饰键说明（Windows）：`ALT` = Super，`ALT|CTRL` = Super+Ctrl。macOS 上 `SUPER` = Cmd。

### 通用 / 复制粘贴

| 按键 | 功能 |
|------|------|
| `F1` | 激活复制模式 |
| `F2` | 命令面板 |
| `F3` | 启动器 |
| `F4` | 标签页导航器 |
| `F11` | 切换全屏 |
| `F12` | 调试面板 |
| `ALT-f` | 搜索 |
| `CTRL-SHIFT-c` | 复制到剪贴板 |
| `CTRL-SHIFT-v` | 从剪贴板粘贴 |

### 标签页

| 按键 | 功能 |
|------|------|
| `ALT-t` | 新建默认域标签页 |
| `ALT|CTRL-t` | 新建 WSL 标签页（在 `config/constants.lua` → `M.WSL` 中自定义发行版/用户名/路径） |
| `ALT-[` / `ALT-]` | 切换到上一个/下一个标签页 |
| `ALT|CTRL-[` / `ALT|CTRL-]` | 向左/右移动标签页 |
| `ALT-q` | 关闭当前面板 |
| `ALT|CTRL-q` | 关闭当前标签页 |
| `CTRL-SHIFT-R` | 重命名当前标签页 |

### 面板

| 按键 | 功能 |
|------|------|
| `ALT|CTRL-/` | 垂直分割 |
| `ALT|CTRL-\` | 水平分割 |
| `ALT|CTRL--` | 关闭当前面板（需确认） |
| `ALT|CTRL-z` | 切换面板缩放 |
| `ALT|CTRL-h/j/k/l` | 左/下/上/右导航面板 |
| `ALT|CTRL-方向键` | 调整面板大小 |

### 字体

| 按键 | 功能 |
|------|------|
| `ALT-↑` | 增大字体 |
| `ALT-↓` | 减小字体 |
| `ALT-r` | 重置字体大小 |

### 引导键（Leader Key）快捷键表

按 `LEADER`（默认为 `CTRL-a`）后：

| 按键 | 功能 |
|------|------|
| `f` | 进入字体调整模式（1 秒超时） |
| `p` | 进入面板大小调整模式（1 秒超时） |

**字体调整模式**（1 秒后自动退出）：

| 按键 | 功能 |
|------|------|
| `k` / `j` | 增大 / 减小字体 |
| `r` | 重置字体大小 |
| `Escape` / `q` | 退出 |

**面板调整模式**（1 秒后自动退出）：

| 按键 | 功能 |
|------|------|
| `k` / `j` | 向上 / 向下调整 |
| `h` / `l` | 向左 / 向右调整 |
| `Escape` / `q` | 退出 |

### 鼠标

| 手势 | 功能 |
|------|------|
| `CTRL` + 左键单击 | 打开光标下的链接 |
| 左键单击 / 拖拽 | 选择单元格 |
| 双击左键 | 选择单词 |
| 三击左键 | 选择整行 |
| 滚轮 | 滚动输出 |

## 如何修改

### 添加新快捷键
1. 打开 `config/bindings/keys.lua`
2. 找到对应的表常量（如 `TAB_BINDINGS`、`PANE_BINDINGS`）
3. 在对应表中添加绑定
4. 或创建新的表常量并在 `combine_bindings()` 中引用

### 修改外观常量
1. 打开 `config/constants.lua`
2. 修改相关常量（如 `WINDOW.INITIAL_COLS`、`COLORS.WINDOW_FRAME`）
3. 修改会自动应用到所有使用这些常量的模块

### 个人覆盖 (user.lua)

创建 wezterm 根目录下的 `user.lua`（已在 gitignore 中），可覆盖任意常量而不修改共享配置：

```lua
local M = {}
M.WSL = {
  DISTRIBUTION = "Ubuntu",
  USERNAME = "你的用户名",
  DEFAULT_CWD = "/home/你的用户名",
  DEFAULT_PROG = { "zsh" },
}
return M
```

可覆盖 `config/constants.lua` 中的任何字段：窗口大小、配色、WSL 域等。参见 `user.lua.example`。

### 添加新外观模块
1. 在 `config/appearance/` 中创建新文件
2. 返回一个包含配置的表
3. 在 `config/appearance.lua` 中导入并合并

### 扩展事件处理
1. 事件处理器在 `events/` 目录中
2. 每个处理器模块导出 `setup()` 函数
3. 处理器在 `wezterm.lua` 中注册

## 此结构的优势

1. **可维护性**：清晰的关注点分离，易于查找和修改特定功能
2. **可扩展性**：易于添加新功能，无需修改现有代码
3. **可读性**：相关配置分组在一起
4. **可复用性**：常量可在模块间复用
5. **可测试性**：每个模块可独立测试
6. **文档性**：清晰的结构本身就是隐式文档
