# ============================================================================
# PowerShell 配置部署脚本
# 使用符号链接方式部署配置（修改源文件后自动生效）
# 注意：需要管理员权限
# ============================================================================

$SourceDir = Split-Path -Parent $MyInvocation.MyCommand.Path
$TargetDir = Split-Path -Parent $PROFILE

Write-Host "`n=== PowerShell 配置部署（符号链接方式） ===" -ForegroundColor Magenta

# 检查管理员权限
$isAdmin = ([Security.Principal.WindowsPrincipal] [Security.Principal.WindowsIdentity]::GetCurrent()).IsInRole([Security.Principal.WindowsBuiltInRole]::Administrator)

if (-not $isAdmin) {
    Write-Host "`n⚠ 警告：创建符号链接需要管理员权限！" -ForegroundColor Red
    Write-Host "请右键点击 PowerShell，选择 '以管理员身份运行' 后重试`n" -ForegroundColor Yellow
    exit 1
}

# 1. 创建 modules 符号链接
Write-Host "`nStep 1: Creating symbolic link for modules..." -ForegroundColor Cyan
$ModulesSource = Join-Path $SourceDir "modules"
$ModulesTarget = Join-Path $TargetDir "modules"

if (Test-Path $ModulesSource) {
    # 删除现有目录或链接
    if (Test-Path $ModulesTarget) {
        Write-Host "  Removing existing: $ModulesTarget" -ForegroundColor Yellow
        Remove-Item $ModulesTarget -Recurse -Force
    }
    
    # 创建符号链接
    New-Item -ItemType SymbolicLink -Path $ModulesTarget -Target $ModulesSource -Force | Out-Null
    Write-Host "✓ Symbolic link created: $ModulesTarget -> $ModulesSource" -ForegroundColor Green
} else {
    Write-Warning "Modules directory not found: $ModulesSource"
}

# 2. 创建 config 符号链接
Write-Host "`nStep 2: Creating symbolic link for config..." -ForegroundColor Cyan
$ConfigSource = Join-Path $SourceDir "config"
$ConfigTarget = Join-Path $TargetDir "config"

if (Test-Path $ConfigSource) {
    # 删除现有目录或链接
    if (Test-Path $ConfigTarget) {
        Write-Host "  Removing existing: $ConfigTarget" -ForegroundColor Yellow
        Remove-Item $ConfigTarget -Recurse -Force
    }
    
    # 创建符号链接
    New-Item -ItemType SymbolicLink -Path $ConfigTarget -Target $ConfigSource -Force | Out-Null
    Write-Host "✓ Symbolic link created: $ConfigTarget -> $ConfigSource" -ForegroundColor Green
} else {
    Write-Warning "Config directory not found: $ConfigSource"
}

# 3. 创建主配置文件符号链接
Write-Host "`nStep 3: Creating symbolic link for profile..." -ForegroundColor Cyan
$ProfileSource = Join-Path $SourceDir "Microsoft.PowerShell_profile.ps1"

if (Test-Path $ProfileSource) {
    try {
        # 备份现有配置（如果不是符号链接）
        if (Test-Path $PROFILE) {
            $item = Get-Item $PROFILE
            if ($item.LinkType -ne "SymbolicLink") {
                $BackupPath = "$PROFILE.backup.$(Get-Date -Format 'yyyyMMdd-HHmmss')"
                Copy-Item $PROFILE $BackupPath -Force
                Write-Host "  Backup created: $BackupPath" -ForegroundColor Yellow
            }
            Remove-Item $PROFILE -Force
        }
        
        # 创建符号链接
        New-Item -ItemType SymbolicLink -Path $PROFILE -Target $ProfileSource -Force | Out-Null
        Write-Host "✓ Symbolic link created: $PROFILE -> $ProfileSource" -ForegroundColor Green
    }
    catch {
        Write-Warning "Cannot create symbolic link for profile (file is in use)."
        Write-Host "`nPlease close all PowerShell windows and run again." -ForegroundColor Yellow
    }
} else {
    Write-Warning "Profile not found: $ProfileSource"
}

# 4. 验证部署
Write-Host "`nStep 4: Verifying symbolic links..." -ForegroundColor Cyan

$checks = @(
    @{Path = $ModulesTarget; Name = "Modules symbolic link"; IsLink = $true},
    @{Path = $ConfigTarget; Name = "Config symbolic link"; IsLink = $true},
    @{Path = $PROFILE; Name = "Profile symbolic link"; IsLink = $true},
    @{Path = (Join-Path $ModulesTarget "git.psm1"); Name = "Git module"},
    @{Path = (Join-Path $ModulesTarget "jujutsu.psm1"); Name = "Jujutsu module"},
    @{Path = (Join-Path $ModulesTarget "utilities.psm1"); Name = "Utilities module"},
    @{Path = (Join-Path $ModulesTarget "system.psm1"); Name = "System module"},
    @{Path = (Join-Path $ModulesTarget "network.psm1"); Name = "Network module"},
    @{Path = (Join-Path $ConfigTarget "settings.psd1"); Name = "Settings file"}
)

$allGood = $true
foreach ($check in $checks) {
    if (Test-Path $check.Path) {
        $item = Get-Item $check.Path
        if ($check.IsLink -and $item.LinkType -eq "SymbolicLink") {
            Write-Host "  ✓ $($check.Name) -> $($item.Target)" -ForegroundColor Green
        } elseif ($check.IsLink -and $item.LinkType -ne "SymbolicLink") {
            Write-Host "  ⚠ $($check.Name) - NOT A SYMBOLIC LINK" -ForegroundColor Yellow
            $allGood = $false
        } else {
            Write-Host "  ✓ $($check.Name)" -ForegroundColor Green
        }
    } else {
        Write-Host "  ✗ $($check.Name) - NOT FOUND" -ForegroundColor Red
        $allGood = $false
    }
}

# 5. 完成
Write-Host "`n=== Deployment " -NoNewline -ForegroundColor Magenta
if ($allGood) {
    Write-Host "Complete" -NoNewline -ForegroundColor Green
} else {
    Write-Host "Incomplete" -NoNewline -ForegroundColor Yellow
}
Write-Host " ===" -ForegroundColor Magenta

Write-Host "`n✨ 符号链接优势：" -ForegroundColor Cyan
Write-Host "  • 修改源文件后自动生效，无需重新部署" -ForegroundColor Green
Write-Host "  • 便于版本控制和同步配置" -ForegroundColor Green
Write-Host "  • 源文件位置：$SourceDir" -ForegroundColor Yellow

Write-Host "`nNext steps:" -ForegroundColor Cyan
Write-Host "  1. 关闭所有 PowerShell 窗口" -ForegroundColor Yellow
Write-Host "  2. 打开新的 PowerShell 窗口" -ForegroundColor Yellow
Write-Host "  3. 配置将自动加载" -ForegroundColor Yellow
Write-Host ""
