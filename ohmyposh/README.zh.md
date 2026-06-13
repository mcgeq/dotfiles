# oh-my-posh 配置

PowerShell 自定义提示符主题，使用菱形块布局。

## 文件

- `mcgeq.omp.toml` — 提示符主题定义（版本 3）

## 布局（3 块）

1. **左侧提示符**（换行）：OS 图标 → 路径 → Git 状态 → Jujutsu 状态
2. **右侧提示符**：系统信息（内存）→ Node.js → PHP → npm → 执行时间 → 时钟
3. **左侧提示符结尾**（换行）：`╰─` 前缀用于命令输入

## 特性

- OS 检测带 Nerd Font 图标（Arch、Windows、WSL 等）
- Git 分支/暂存/变更指示器，颜色编码状态
- Jujutsu Change ID 显示
- 执行时间跟踪（阈值：150ms）
- 实时时钟
- 包管理器版本显示（Node、PHP、npm）

详见 [mcgeq.omp.toml](mcgeq.omp.toml)。
