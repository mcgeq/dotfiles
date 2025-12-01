<#
.SYNOPSIS
    迁移 Emacs 扩展到新的目录结构

.DESCRIPTION
    此脚本将重新组织 site-lisp/extensions/ 目录，
    按功能分类整理所有 Git submodules。

.NOTES
    Author: mcge
    Date: 2025-12-01
    Version: 1.0
#>

param(
    [switch]$DryRun,
    [switch]$Backup,
    [string]$BackupPath = ".\backup"
)

# 错误处理
$ErrorActionPreference = "Stop"

# 颜色输出函数
function Write-ColorOutput {
    param(
        [string]$Message,
        [string]$Color = "White"
    )
    Write-Host $Message -ForegroundColor $Color
}

# 检查 Git 状态
function Test-GitClean {
    $status = git status --porcelain
    if ($status) {
        Write-ColorOutput "警告: Git 工作目录不干净，请先提交或暂存更改" -Color Yellow
        Write-ColorOutput "运行 'git status' 查看详情" -Color Yellow
        return $false
    }
    return $true
}

# 创建备份
function Backup-Current {
    param([string]$Path)
    
    if ($Backup) {
        Write-ColorOutput "创建备份..." -Color Cyan
        
        if (Test-Path $Path) {
            Remove-Item $Path -Recurse -Force
        }
        New-Item -ItemType Directory -Path $Path -Force | Out-Null
        
        # 备份 .gitmodules
        Copy-Item .\.gitmodules "$Path\.gitmodules" -Force
        
        # 备份 extensions 目录结构
        Copy-Item .\emacs\site-lisp\extensions "$Path\extensions" -Recurse -Force
        
        Write-ColorOutput "备份完成: $Path" -Color Green
    }
}

# 插件分类映射
$ExtensionCategories = @{
    # Core - 核心库
    "dash" = "core"
    "s.el" = "core"
    "f.el" = "core"
    "ht" = "core"
    "popup-el" = "core"
    "lazy-load" = "core"
    
    # Completion - 补全系统
    "vertico" = "completion"
    "marginalia" = "completion"
    "orderless" = "completion"
    "embark" = "completion"
    "consult" = "completion"
    "pinyinlib" = "completion"
    
    # Editor - 编辑器增强
    "beacon" = "editor"
    "symbol-overlay" = "editor"
    "vundo" = "editor"
    "fingertip" = "editor"
    "wraplish" = "editor"
    "markmacro" = "editor"
    "ws-butler" = "editor"
    
    # Git - 版本控制
    "magit" = "git"
    "with-editor" = "git"
    "jujutsu" = "git"
    "llama" = "git"
    
    # LSP - LSP和语言支持（保持子目录结构）
    "lsp" = "lsp"
    
    # Org - Org Mode（保持子目录结构）
    "org" = "org"
    
    # UI - 用户界面
    "doom-modeline" = "ui"
    "shrink-path" = "ui"
    "solaire-mode" = "ui"
    "themes" = "ui"
    
    # Search - 搜索工具
    "blink-search" = "search"
    "color-rg" = "search"
    "consult-todo" = "search"
    "hl-todo" = "search"
    
    # Snippets - 代码片段（保持子目录结构）
    "snippets" = "snippets"
    
    # Input - 输入法
    "emacs-rime" = "input"
    "posframe" = "input"
    
    # Docs - 文档工具
    "markdown-ts-mode" = "docs"
    "md" = "docs"
    "flymake-vale" = "docs"
    "emacs-htmlize" = "docs"
    
    # Utils - 实用工具
    "auto-save" = "utils"
    "sort-tab" = "utils"
    "helpful" = "utils"
    "elisp-refs" = "utils"
    "hydra" = "utils"
    "instant-rename-tag" = "utils"
    "highlight-matching-tag" = "utils"
    "json-mode" = "utils"
    "json-snatcher" = "utils"
    "js2-mode" = "utils"
}

# 获取当前所有扩展
function Get-CurrentExtensions {
    $extensionsPath = ".\emacs\site-lisp\extensions"
    
    if (-not (Test-Path $extensionsPath)) {
        Write-ColorOutput "错误: 找不到 extensions 目录" -Color Red
        exit 1
    }
    
    Get-ChildItem $extensionsPath -Directory | ForEach-Object { $_.Name }
}

# 生成迁移映射
function Get-MigrationMap {
    $currentExtensions = Get-CurrentExtensions
    $migrationMap = @{}
    
    foreach ($ext in $currentExtensions) {
        $category = $ExtensionCategories[$ext]
        
        if (-not $category) {
            Write-ColorOutput "警告: 扩展 '$ext' 未分类，跳过" -Color Yellow
            continue
        }
        
        $oldPath = "emacs/site-lisp/extensions/$ext"
        $newPath = "emacs/site-lisp/extensions/$category/$ext"
        
        $migrationMap[$oldPath] = $newPath
    }
    
    return $migrationMap
}

# 处理嵌套扩展（lsp/, org/, themes/ 等）
function Get-NestedExtensions {
    $nestedMap = @{}
    
    # LSP 子目录
    $lspPath = ".\emacs\site-lisp\extensions\lsp"
    if (Test-Path $lspPath) {
        Get-ChildItem $lspPath -Directory | ForEach-Object {
            $oldPath = "emacs/site-lisp/extensions/lsp/$($_.Name)"
            $newPath = "emacs/site-lisp/extensions/lsp/$($_.Name)"
            $nestedMap[$oldPath] = $newPath
        }
    }
    
    # Org 子目录
    $orgPath = ".\emacs\site-lisp\extensions\org"
    if (Test-Path $orgPath) {
        Get-ChildItem $orgPath -Directory | ForEach-Object {
            $oldPath = "emacs/site-lisp/extensions/org/$($_.Name)"
            $newPath = "emacs/site-lisp/extensions/org/$($_.Name)"
            $nestedMap[$oldPath] = $newPath
        }
    }
    
    # Themes 子目录
    $themesPath = ".\emacs\site-lisp\extensions\themes"
    if (Test-Path $themesPath) {
        Get-ChildItem $themesPath -Directory | ForEach-Object {
            $oldPath = "emacs/site-lisp/extensions/themes/$($_.Name)"
            $newPath = "emacs/site-lisp/extensions/ui/themes/$($_.Name)"
            $nestedMap[$oldPath] = $newPath
        }
    }
    
    # MD 子目录
    $mdPath = ".\emacs\site-lisp\extensions\md"
    if (Test-Path $mdPath) {
        Get-ChildItem $mdPath -Directory | ForEach-Object {
            $oldPath = "emacs/site-lisp/extensions/md/$($_.Name)"
            $newPath = "emacs/site-lisp/extensions/docs/$($_.Name)"
            $nestedMap[$oldPath] = $newPath
        }
    }
    
    # Snippets 子目录
    $snippetsPath = ".\emacs\site-lisp\extensions\snippets"
    if (Test-Path $snippetsPath) {
        Get-ChildItem $snippetsPath -Directory | ForEach-Object {
            $oldPath = "emacs/site-lisp/extensions/snippets/$($_.Name)"
            $newPath = "emacs/site-lisp/extensions/snippets/$($_.Name)"
            $nestedMap[$oldPath] = $newPath
        }
    }
    
    return $nestedMap
}

# 执行迁移
function Invoke-Migration {
    param(
        [hashtable]$MigrationMap,
        [bool]$IsDryRun
    )
    
    $totalCount = $MigrationMap.Count
    $currentCount = 0
    
    Write-ColorOutput "`n开始迁移 $totalCount 个扩展..." -Color Cyan
    
    foreach ($entry in $MigrationMap.GetEnumerator()) {
        $currentCount++
        $oldPath = $entry.Key
        $newPath = $entry.Value
        
        Write-Progress -Activity "迁移扩展" -Status "$currentCount / $totalCount" `
            -PercentComplete (($currentCount / $totalCount) * 100)
        
        if ($oldPath -eq $newPath) {
            Write-ColorOutput "  [$currentCount/$totalCount] 跳过 (无需移动): $oldPath" -Color Gray
            continue
        }
        
        if ($IsDryRun) {
            Write-ColorOutput "  [$currentCount/$totalCount] [DRY-RUN] $oldPath -> $newPath" -Color Yellow
        } else {
            try {
                # 创建目标目录
                $newDir = Split-Path $newPath -Parent
                if (-not (Test-Path $newDir)) {
                    New-Item -ItemType Directory -Path $newDir -Force | Out-Null
                }
                
                # 使用 git mv 移动 submodule
                Write-ColorOutput "  [$currentCount/$totalCount] 移动: $oldPath -> $newPath" -Color Green
                git mv $oldPath $newPath 2>&1 | Out-Null
                
                if ($LASTEXITCODE -ne 0) {
                    Write-ColorOutput "    警告: git mv 失败，尝试手动移动" -Color Yellow
                    Move-Item $oldPath $newPath -Force
                }
            } catch {
                Write-ColorOutput "    错误: 移动失败 - $_" -Color Red
            }
        }
    }
    
    Write-Progress -Activity "迁移扩展" -Completed
}

# 更新 .gitmodules
function Update-GitModules {
    param([hashtable]$MigrationMap)
    
    Write-ColorOutput "`n更新 .gitmodules..." -Color Cyan
    
    $gitmodulesPath = ".\.gitmodules"
    if (-not (Test-Path $gitmodulesPath)) {
        Write-ColorOutput "错误: 找不到 .gitmodules 文件" -Color Red
        return
    }
    
    $content = Get-Content $gitmodulesPath -Raw
    
    foreach ($entry in $MigrationMap.GetEnumerator()) {
        $oldPath = $entry.Key
        $newPath = $entry.Value
        
        if ($oldPath -ne $newPath) {
            $content = $content -replace [regex]::Escape("path = $oldPath"), "path = $newPath"
        }
    }
    
    if (-not $DryRun) {
        Set-Content $gitmodulesPath -Value $content -NoNewline
        Write-ColorOutput ".gitmodules 已更新" -Color Green
    } else {
        Write-ColorOutput "[DRY-RUN] .gitmodules 将被更新" -Color Yellow
    }
}

# 同步 Git Submodules
function Sync-GitSubmodules {
    Write-ColorOutput "`n同步 Git Submodules..." -Color Cyan
    
    if (-not $DryRun) {
        git submodule sync 2>&1 | Out-Null
        if ($LASTEXITCODE -eq 0) {
            Write-ColorOutput "Submodules 同步完成" -Color Green
        } else {
            Write-ColorOutput "警告: Submodules 同步失败" -Color Yellow
        }
    } else {
        Write-ColorOutput "[DRY-RUN] 将执行: git submodule sync" -Color Yellow
    }
}

# 生成迁移报告
function New-MigrationReport {
    param([hashtable]$MigrationMap)
    
    $reportPath = ".\emacs\MIGRATION_REPORT.txt"
    $report = @"
Emacs 扩展迁移报告
生成时间: $(Get-Date -Format 'yyyy-MM-dd HH:mm:ss')
==========================================

迁移的扩展数量: $($MigrationMap.Count)

详细迁移列表:
"@
    
    foreach ($entry in $MigrationMap.GetEnumerator() | Sort-Object Value) {
        $report += "`n$($entry.Key)`n  -> $($entry.Value)"
    }
    
    $report += "`n`n按类别统计:"
    $categories = $MigrationMap.Values | ForEach-Object { ($_ -split '/')[3] } | Group-Object
    foreach ($cat in $categories | Sort-Object Name) {
        $report += "`n  $($cat.Name): $($cat.Count) 个扩展"
    }
    
    Set-Content $reportPath -Value $report
    Write-ColorOutput "`n迁移报告已生成: $reportPath" -Color Green
}

# 主函数
function Main {
    Write-ColorOutput "==================================" -Color Cyan
    Write-ColorOutput "Emacs 扩展迁移脚本" -Color Cyan
    Write-ColorOutput "==================================" -Color Cyan
    
    # 检查是否在正确的目录
    if (-not (Test-Path ".\emacs\site-lisp\extensions")) {
        Write-ColorOutput "错误: 请在 dotfiles 根目录运行此脚本" -Color Red
        exit 1
    }
    
    # 检查 Git 状态
    if (-not $DryRun -and -not (Test-GitClean)) {
        $continue = Read-Host "是否继续？(y/N)"
        if ($continue -ne 'y') {
            Write-ColorOutput "已取消迁移" -Color Yellow
            exit 0
        }
    }
    
    # 创建备份
    if ($Backup) {
        Backup-Current $BackupPath
    }
    
    # 生成迁移映射
    Write-ColorOutput "`n分析扩展目录..." -Color Cyan
    $migrationMap = Get-MigrationMap
    $nestedMap = Get-NestedExtensions
    
    # 合并映射
    foreach ($entry in $nestedMap.GetEnumerator()) {
        $migrationMap[$entry.Key] = $entry.Value
    }
    
    Write-ColorOutput "找到 $($migrationMap.Count) 个扩展" -Color Green
    
    if ($DryRun) {
        Write-ColorOutput "`n=== DRY RUN 模式 ===" -Color Yellow
        Write-ColorOutput "不会执行实际的文件操作" -Color Yellow
    }
    
    # 执行迁移
    Invoke-Migration -MigrationMap $migrationMap -IsDryRun $DryRun
    
    # 更新 .gitmodules
    Update-GitModules -MigrationMap $migrationMap
    
    # 同步 submodules
    Sync-GitSubmodules
    
    # 生成报告
    New-MigrationReport -MigrationMap $migrationMap
    
    # 完成
    Write-ColorOutput "`n==================================" -Color Green
    if ($DryRun) {
        Write-ColorOutput "DRY RUN 完成！" -Color Yellow
        Write-ColorOutput "运行不带 -DryRun 参数来执行实际迁移" -Color Yellow
    } else {
        Write-ColorOutput "迁移完成！" -Color Green
        Write-ColorOutput "`n后续步骤:" -Color Cyan
        Write-ColorOutput "1. 运行: git submodule update --init --recursive" -Color White
        Write-ColorOutput "2. 更新配置文件（init-loadpath.org）" -Color White
        Write-ColorOutput "3. 重新生成配置: make clean && make generate" -Color White
        Write-ColorOutput "4. 测试 Emacs 配置" -Color White
    }
    Write-ColorOutput "==================================" -Color Green
}

# 运行主函数
Main
