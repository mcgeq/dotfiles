[English](README.md)

# Fish Shell 配置

轻量级 fish 配置，作为跨平台备用 shell，主要在 Arch Linux（WSL）上使用。

## 文件

- `config.fish` — 交互式 Shell 配置

## 功能

- **Emacs**：`e` → `emacs -nw`，PATH 包含 `~/emacs/bin`
- **Arch 包管理**：`pry`（paru 更新）、`prs`（搜索）、`pri`（安装）、`prr`（移除）
- **Git 缩写**：`gad`、`gpl`、`gph`、`gst`、`gsi`、`gsh`、`gsm`
- **Cargo**：`~/.cargo/bin` 加入 PATH
- **Starship**：已初始化提示符
- **pnpm**：`PNPM_HOME` 加入 PATH
- **tmux**：登录时自动启动 tmux 会话 `main`（如不在 tmux 中）

详见 [config.fish](config.fish)。
