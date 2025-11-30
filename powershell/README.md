# PowerShell 配置 - 模块化版本

[![PowerShell](https://img.shields.io/badge/PowerShell-7.5+-blue?logo=powershell)](https://github.com/PowerShell/PowerShell)
[![License](https://img.shields.io/badge/License-MIT-green)](LICENSE)

> 高度模块化、易于维护和扩展的 PowerShell 配置

![screenshot](screenshots/screenshot-1.png "config")

---

## ✨ 特性

- 🧩 **模块化架构** - 功能按模块组织，易于维护
- 📦 **自动加载** - 模块按需加载，提升性能
- 🎨 **Starship 提示符** - 美观且高度可定制
- 🔧 **丰富的工具集** - Git、Jujutsu、系统管理、网络工具
- ⚡ **智能补全** - PSReadLine 历史预测
- 🌐 **多包管理器支持** - pip、Chocolatey、Scoop、Winget

---

## 📁 目录结构

```
powershell/
├── Microsoft.PowerShell_profile.ps1       # 主配置文件
├── modules/                                # 功能模块
│   ├── git.psm1                           # Git 操作
│   ├── jujutsu.psm1                       # Jujutsu 操作
│   ├── utilities.psm1                     # 工具函数
│   ├── system.psm1                        # 系统管理
│   └── network.psm1                       # 网络管理
├── config/                                 # 配置文件
│   └── settings.psd1                      # 全局配置
└── README.md                               # 文档
```

---

## 🚀 快速开始

### 1. 安装依赖

```powershell
# 必需模块
Install-Module PSReadLine -Force
Install-Module Terminal-Icons -Force

# 可选工具
winget install starship         # 提示符
winget install ajeetdsouza.fnm  # Node.js 版本管理
scoop install scoop-search      # Scoop 搜索增强
```

### 2. 启用配置

```powershell
# 重新加载配置
. $PROFILE

# 或重启 PowerShell
```

---

## 📚 模块说明

### Git 模块 (`git.psm1`)

| 函数 | 别名 | 说明 |
|------|------|------|
| `Invoke-GitAdd` | `gad` | 暂存所有更改 |
| `Invoke-GitStatus` | `gst` | 查看状态 |
| `Invoke-GitPush` | `gph` | 推送到远程 |
| `Invoke-GitPull` | `gpl` | 从远程拉取 |
| `Invoke-GitCommit` | `gco` | 提交更改 |
| `New-GitBranch` | `gcn` | 创建新分支 |
| `Switch-GitBranch` | `gch` | 切换分支 |
| `Get-GitBranches` | `gbr` | 列出分支 |
| `Merge-GitBranch` | `gme` | 合并分支 |
| `Sync-GitSubmodules` | `gsync` | 同步所有子模块 |
| `Invoke-ConventionalCommit` | `gcz` | Commitizen 提交 |

**Submodule 管理：**
- `gsi` - 初始化子模块
- `gspl` - 更新子模块
- `gsync` - 一键同步所有子模块

### Jujutsu 模块 (`jujutsu.psm1`)

| 函数 | 别名 | 说明 |
|------|------|------|
| `Invoke-JujutsuAdd` | `jad` | 添加文件 |
| `Get-JujutsuStatus` | `jst` | 查看状态 |
| `Invoke-JujutsuCommit` | `jco` | 提交更改 |
| `Get-JujutsuLog` | `jlg` | 查看日志 |
| `Invoke-JujutsuGitPush` | `jjp` | 推送到 Git |
| `Sync-JujutsuMain` | `jfm` | 同步到 main |

### 工具函数模块 (`utilities.psm1`)

| 函数 | 别名 | 说明 |
|------|------|------|
| `New-Directory` | `mk` | 创建目录 |
| `New-File` | `touch` | 创建/更新文件 |
| `Remove-ItemSafe` | `rm` | 安全删除 |
| `Open-ExplorerHere` | `open` | 打开资源管理器 |
| `Get-DirectoryList` | `ls` | 列出目录 |
| `Invoke-PdmPython` | `pru` | PDM Python |
| `Invoke-UvPython` | `uvr` | UV Python |

### 系统管理模块 (`system.psm1`)

| 函数 | 别名 | 说明 |
|------|------|------|
| `Update-AllPackages` | `os-update` | 更新所有包管理器 |
| `Get-SystemInfo` | `sysinfo` | 系统信息 |

### 网络管理模块 (`network.psm1`)

| 函数 | 别名 | 说明 |
|------|------|------|
| `Set-SystemProxy` | - | 设置代理 |
| `Remove-SystemProxy` | - | 移除代理 |
| `Get-NetworkAdapters` | `getnic` | 网络适配器 |
| `Get-ActiveIPv4Routes` | `getip` | IPv4 路由 |
| `Get-ActiveIPv6Routes` | `getip6` | IPv6 路由 |
| `Get-PublicIP` | `myip` | 公网 IP |

---

## ⚙️ 配置说明

编辑 `config/settings.psd1` 来自定义配置：

```powershell
@{
    # 代理设置
    Proxy = @{
        Url = "http://localhost:7897"
        Enabled = $false
    }
    
    # Git 配置
    Git = @{
        DefaultBranch = "main"
        SubmoduleBranches = @("main", "master", "trunk")
    }
    
    # 模块开关
    Modules = @{
        PSReadLine = $true
        TerminalIcons = $true
        Starship = $true
        ScoopSearch = $true
        Fnm = $true
    }
}
```

---

## 🎯 使用示例

### Git 工作流

```powershell
# 基础操作
gad                   # 暂存所有更改
gco "feat: new feature"  # 提交
gph                   # 推送

# 分支管理
gcn feature/new-ui    # 创建新分支
gch main              # 切换到 main
gme feature/new-ui    # 合并分支

# Submodule 管理
gsync                 # 一键同步所有子模块
```

### Jujutsu 工作流

```powershell
jad                   # 添加更改
jco "feat: new feature"  # 提交
jfm                   # 同步到 main
jjp                   # 推送到 Git
```

### 系统管理

```powershell
os-update             # 更新所有包管理器
sysinfo               # 查看系统信息
myip                  # 查看公网 IP
```

---

## 🔧 自定义扩展

### 添加新模块

1. 在 `modules/` 目录创建新模块文件：

```powershell
# modules/custom.psm1
function Get-CustomInfo {
    Write-Host "`n✨ PowerShell Profile Loaded (Modular Edition)" -ForegroundColor Green
}

Export-ModuleMember -Function 'Get-CustomInfo'
```

2. 在主配置文件中导入：

```powershell
Import-Module (Join-Path $ModulesDir "custom.psm1")
```

### 添加别名

在主配置文件的 `#region 别名定义` 部分添加：

```powershell
Set-Alias -Name myalias -Value Get-CustomInfo
```

---

## 📦 依赖项

### 必需

- PowerShell 7.5+
- PSReadLine
- Terminal-Icons

### 可选

- Starship (提示符美化)
- fnm (Node.js 版本管理)
- scoop-search (Scoop 增强)
- Git / Jujutsu (版本控制)

---

## 📝 更新日志

### 2025-11-30
- ✨ 模块化重构
- � 拆分为 5 个功能模块 (Git, Jujutsu, Utilities, System, Network)
- ⚙️ 外部配置文件支持
- 📚 完善文档和注释
- 🐛 统一函数命名规范（Verb-Noun）
- 🔒 增强错误处理
- 🚀 提升可维护性和可扩展性

---

## 📄 许可证

MIT License

---

## 👤 作者

**mcge** <<mcgeq@outlook.com>>

---

## 🙏 致谢

- [PowerShell](https://github.com/PowerShell/PowerShell)
- [Starship](https://starship.rs/)
- [PSReadLine](https://github.com/PowerShell/PSReadLine)
- [Terminal-Icons](https://github.com/devblackops/Terminal-Icons)
