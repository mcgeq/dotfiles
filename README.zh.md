[English](README.md)

# Dotfiles

个人跨平台开发环境配置文件。以 Windows 为主，覆盖多 Shell 和多编辑器。

## 概览

| 类别 | 组件 | 说明 | 文档 |
|------|------|------|------|
| **Shell** | [PowerShell](powershell/) | 模块化 `$PROFILE`，提示符，别名，PSReadLine | [README](powershell/README.md) |
| | [Nushell](nushell/) | （已弃用 — 现主要使用 pwsh） | [README](nushell/README.md) |
| | [Fish](fish/) | 轻量级跨平台备用 | — |
| **终端** | [WezTerm](wezterm/) | 模块化 wezterm 配置，标签栏，快捷键，主题 | [README](wezterm/README.md) |
| | [Alacritty](alacritty/) | GPU 加速终端配置 | — |
| | [tmux](tmux/) | 终端复用器配置 | — |
| **提示符** | [Starship](starship/) | 跨 Shell 提示符（pwsh / nushell / fish） | [README](starship/README.md) |
| | [oh-my-posh](ohmyposh/) | 备选提示符主题 | — |
| **编辑器** | [Neovim](nvim/) | Neovim 0.12+ 配置，`vim.pack`，snacks，blink.cmp | [README](nvim/README.md) |
| | [Vim](vimrc/) | 传统 vimrc，pack 插件 | [README](vimrc/README.md) |
| | [Emacs](emacs/) | Emacs 配置，lsp-bridge，org-mode | [README](emacs/README.org) |
| | [Zed](Zed/) | Zed 编辑器设置、代码片段、快捷键 | [README](Zed/README.md) |
| **Git** | [Gitconfig](gitconfig/) | 全局 git 配置（用户、别名、diff、delta） | [README](gitconfig/README.md) |
| | [Jujutsu](jj/) | `jj` 版本控制配置 | — |
| | [Lazygit](lazygit/) | Git 终端 UI | — |
| **包管理** | [Scoop](scoop/) | Windows 包管理器桶/应用配置 | [README](scoop/README.md) |

### 自动化与工具

| 组件 | 说明 |
|------|------|
| [AutoKey](autokey/) | Linux 桌面自动化（热字符串、宏） |
| [lf](lf/) | 终端文件管理器配置 |
| [commit/](commit/) | Commitizen + commitlint + husky，标准化提交信息 |
| [init.ps1](init.ps1) | 引导脚本 — 为所有配置组件创建符号链接 |
| [deinit.ps1](deinit.ps1) | 卸载脚本 — 移除 init.ps1 创建的符号链接 |
| [install.bat](install.bat) | 传统安装入口 |

## 快速开始

```powershell
# 克隆到统一位置（如 ~/dotfiles 或 D:\config\dotfiles）
git clone <repo-url> D:\config\dotfiles

# 引导所有配置符号链接
.\init.ps1 -All
```

引导脚本会读取 `.gitmodules` 和 `.gitattributes` 检测组件，在其预期位置创建符号链接（如 `$env:USERPROFILE\.config\wezterm` → `wezterm\`）。

## 目录结构

```
dotfiles/
├── init.ps1             # 引导（创建符号链接）
├── deinit.ps1           # 卸载（移除符号链接）
├── install.bat          # 传统安装器
├── .gitmodules          # 插件子模块
├── alacritty/           # Alacritty 终端
├── autokey/             # AutoKey（Linux）
├── commit/              # Commitizen 配置
├── emacs/               # Emacs（site-lisp + config-org）
├── fish/                # Fish shell
├── gitconfig/           # 全局 .gitconfig
├── jj/                  # Jujutsu 版本控制
├── lazygit/             # Lazygit 终端 UI
├── lf/                  # lf 文件管理器
├── nushell/             # Nushell（已弃用）
├── nvim/                # Neovim 0.12+
├── ohmyposh/            # oh-my-posh 提示符
├── powershell/          # PowerShell $PROFILE
├── scoop/               # Scoop 配置
├── starship/            # Starship 提示符
├── tmux/                # tmux（跨平台）
├── vimrc/               # 传统 Vim
├── wezterm/             # WezTerm
└── Zed/                 # Zed 编辑器
```

## 平台说明

- **主操作系统**：Windows（PowerShell 7.5+）
- **跨平台组件**：WezTerm、Neovim、Vim、Emacs、Starship、Git、tmux、fish
- **仅 Linux**：AutoKey
- **macOS/Linux 备用**：Fish shell、tmux
