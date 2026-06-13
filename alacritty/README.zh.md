# Alacritty 配置

GPU 加速终端模拟器配置。使用 JetBrainsMono Nerd Font 字体，集成 WSL Arch，Catppuccin Mocha 配色。

## 文件

```
alacritty/
├── alacritty.toml       # 主配置：窗口、字体、光标、滚动、Shell、环境变量
├── colors.toml           # Catppuccin Mocha 配色方案
├── keybindings.toml      # 自定义快捷键
├── README.md             # 英文文档
└── README.zh.md          # 本文件
```

## 要点

- **字体**：JetBrainsMono Nerd Font，12pt
- **Shell**：WSL Arch（`wsl -d Arch -e fish`）
- **主题**：Catppuccin Mocha，自定义背景色 `#1f1f28`，与 Wezterm 配色一致
- **透明度**：0.95，带模糊效果（仅 Windows 11）
- **窗口**：自动跟随系统主题（`decorations_theme_variant = "System"`），动态内边距
- **光标**：Beam 样式，闪烁，失焦时不中空
- **滚动**：10000 行，3 倍速
- **OSC 52**：支持剪贴板同步
- **TERM**：`alacritty`（官方推荐值，支持真彩色）
- **模块化**：颜色和快捷键作为独立文件导入

## 快捷键

### 通用

| 按键 | 功能 |
|------|------|
| `Ctrl+Shift+N` | 新建窗口 |
| `F11` | 切换全屏 |
| `Ctrl+F11` | 切换最大化 |
| `Ctrl+Shift+H` | 隐藏窗口 |
| `Ctrl+Shift+M` | 最小化窗口 |
| `Ctrl+Shift+Q` | 退出 Alacritty |

### 复制 / 粘贴 / 选择

| 按键 | 功能 |
|------|------|
| `Ctrl+Shift+C` | 复制（内置默认） |
| `Ctrl+Shift+V` | 粘贴（内置默认） |
| `Ctrl+右键` | 粘贴 |
| `Escape` | 清除选择 |

### 搜索

| 按键 | 功能 |
|------|------|
| `Ctrl+Shift+F` | 向前搜索 |
| `Ctrl+Shift+B` | 向后搜索 |
| `Enter`（搜索模式） | 确认搜索 |
| `Escape`（搜索模式） | 取消搜索 |
| `U`（搜索模式） | 清除搜索 |

### 滚动

| 按键 | 功能 |
|------|------|
| `PageUp` | 向上翻页 |
| `PageDown` | 向下翻页 |
| `Home` | 滚动到顶部 |
| `End` | 滚动到底部 |
| `Ctrl+L` | 清除历史/回滚 |

### 链接提示

| 按键 | 功能 |
|------|------|
| `Ctrl+Shift+O` | 复制光标下的 URL 到剪贴板 |

## 配色

基于 Catppuccin Mocha，为与 Wezterm 配色一致做了调整：

- **背景**：`#1f1f28`（比标准 `#1e1e2e` 略亮）
- **前景**：`#cdd6f4`
- **光标**：玫瑰水色 `#f5e0dc`
- **选择**：Surface2 `#585b70`
- **提示**：黄色底 `#f9e2af` / 红色底 `#f38ba8`
