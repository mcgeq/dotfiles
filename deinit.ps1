param(
    [Parameter()]
    [string]$Prefix = "",

    [Parameter()]
    [string]$PathPrefix = "",

    [Parameter()]
    [switch]$All,

    [Parameter()]
    [switch]$DryRun,

    [Parameter()]
    [switch]$Force
)

$ErrorActionPreference = "Stop"
function Write-Cyan { param($msg) Write-Host $msg -ForegroundColor Cyan }
function Write-Yellow { param($msg) Write-Host $msg -ForegroundColor Yellow }
function Write-Green { param($msg) Write-Host $msg -ForegroundColor Green }
function Write-Red { param($msg) Write-Host $msg -ForegroundColor Red }
function Write-Gray { param($msg) Write-Host $msg -ForegroundColor Gray }

function Get-GitmodulesData {
    $content = Get-Content -Path ".gitmodules" -Raw
    $submodules = @()
    $current = $null
    
    foreach ($line in ($content -split "`n")) {
        # 允许行首空白，并捕获子模块名称
        if ($line -match '^\s*\[submodule "(.+)"\]\s*$') {
            if ($current) { $submodules += $current }
            $current = @{
                Name = $matches[1]
                Path = ""
                Url = ""
            }
        }
        # 允许行首和等号周围的空白，并捕获键值对
        elseif ($line -match '^\s*(\w+)\s*=\s*(.+)$') {
            if ($current) {
                $current[$matches[1]] = $matches[2].Trim()
            }
        }
    }
    if ($current) { $submodules += $current }
    
    return $submodules
}

function Get-MatchingSubmodules {
    param(
        [array]$Submodules,
        [string]$Prefix,
        [string]$PathPrefix
    )
    
    $filtered = $Submodules
    
    if ($Prefix) {
        $filtered = $filtered | Where-Object { $_.Name -like "$Prefix*" }
    }
    
    if ($PathPrefix) {
        $filtered = $filtered | Where-Object { $_.Path -like "$PathPrefix*" }
    }
    
    return $filtered
}
function Remove-Submodule {
    param(
        [hashtable]$Submodule,
        [switch]$DryRun
    )
    
    $name = $Submodule.Name
    $path = $Submodule.Path
    
    if ($DryRun) {
        Write-Yellow "[DRY-RUN] Would remove: $name (path: $path)"
        return $true
    }
    
    Write-Yellow "Removing: $name"
    
    try {
        # 1. 尝试 deinit 子模块
        Write-Gray "  Running: git submodule deinit -f -- $path"
        $process = Start-Process -FilePath "git" -ArgumentList "submodule", "deinit", "-f", "--", $path -NoNewWindow -Wait -PassThru
        if ($process.ExitCode -ne 0) {
            Write-Gray "  Note: git submodule deinit returned $($process.ExitCode)"
        }
        
        # 2. 从索引中删除
        Write-Gray "  Running: git rm -f --cached $path"
        $process = Start-Process -FilePath "git" -ArgumentList "rm", "-f", "--cached", $path -NoNewWindow -Wait -PassThru
        if ($process.ExitCode -ne 0) {
            Write-Red "  Error: failed to remove from index: $path"
            return $false
        }
        
        # 3. 删除工作目录
        if (Test-Path $path) {
            Write-Gray "  Removing directory: $path"
            Remove-Item -Path $path -Recurse -Force -ErrorAction SilentlyContinue
            if (Test-Path $path) {
                Write-Red "  Warning: could not completely remove directory: $path"
            }
        }
        
        # 4. 尝试从 git 配置中移除子模块配置节
        # 可能使用的配置节名称
        $possibleConfigNames = @(
            $name,  # 原始名称，如 "emacs-dash"
            $path.Replace('/', '-').Replace('\', '-'),  # 路径转换，如 "emacs-site-lisp-extensions-core-dash"
            $path.Replace('/', '.').Replace('\', '.')   # 路径用点分隔，如 "emacs.site-lisp.extensions.core.dash"
        )
        
        $configRemoved = $false
        foreach ($configName in $possibleConfigNames) {
            Write-Gray "  Checking config section: submodule.$configName"
            # 检查配置节是否存在
            try {
                $output = git config --get-regexp "^submodule\.$configName\." 2>$null
                if ($LASTEXITCODE -eq 0) {
                    # 配置节存在，尝试移除
                    Write-Gray "  Removing config section: submodule.$configName"
                    # 直接执行命令，不捕获输出
                    git config --remove-section "submodule.$configName" 2>$null
                    if ($LASTEXITCODE -eq 0) {
                        Write-Gray "  Successfully removed config section: submodule.$configName"
                        $configRemoved = $true
                    } else {
                        Write-Gray "  Could not remove config section: submodule.$configName"
                    }
                }
            } catch {
                # 忽略错误，继续尝试下一个
            }
        }
        
        if (-not $configRemoved) {
            Write-Gray "  No config sections found to remove"
        }
        
        # 5. 删除 .git/modules 目录
        $possibleModulesPaths = @(
            ".git/modules/$name",
            ".git/modules/$($path.Replace('/', '-').Replace('\', '-'))",
            ".git/modules/$($path.Replace('/', '.').Replace('\', '.'))"
        )
        
        foreach ($modulesPath in $possibleModulesPaths) {
            if (Test-Path $modulesPath) {
                Write-Gray "  Removing modules directory: $modulesPath"
                Remove-Item -Path $modulesPath -Recurse -Force -ErrorAction SilentlyContinue
                if (Test-Path $modulesPath) {
                    Write-Gray "  Warning: could not completely remove: $modulesPath"
                }
            }
        }
        
        # 6. 清理 .git/config 中可能的残留配置
        Write-Gray "  Cleaning any remaining submodule references in .git/config"
        try {
            # 获取所有子模块配置节
            $output = git config --file .git/config --get-regexp '^submodule\.' 2>$null
            if ($output) {
                foreach ($line in $output) {
                    if ($line -match '^submodule\.([^\.]+)\.') {
                        $configKey = $matches[1]
                        if ($configKey -eq $name -or $configKey -like "*$name*" -or $configKey -like "*$($path.Replace('/', '-'))*") {
                            Write-Gray "    Removing: submodule.$configKey.*"
                            git config --file .git/config --remove-section "submodule.$configKey" 2>$null
                        }
                    }
                }
            }
        } catch {
            Write-Gray "    Could not clean .git/config references"
        }
        
        Write-Green "  ✓ Successfully removed: $name"
        return $true
    }
    catch {
        Write-Red "  Error: $_"
        return $false
    }
}
function Update-GitmodulesFile {
    param(
        [array]$RemovedSubmodules
    )
    
    $gitmodulesPath = ".gitmodules"
    if (-not (Test-Path $gitmodulesPath)) {
        Write-Gray ".gitmodules file not found, skipping update."
        return
    }
    
    $content = Get-Content -Path $gitmodulesPath -Raw
    if (-not $content) {
        Write-Gray ".gitmodules file is empty, skipping update."
        return
    }
    
    $newContent = $content
    foreach ($sub in $RemovedSubmodules) {
        $name = $sub.Name
        # 使用正则表达式匹配子模块部分（从 [submodule "name"] 到下一个子模块或文件结尾）
        $pattern = '(?s)\[\s*submodule\s*"' + [regex]::Escape($name) + '"\s*\].*?(?=\[\s*submodule\s*"|$)'
        $newContent = $newContent -replace $pattern, ''
    }
    
    # 清理多余空行和首尾空白
    $newContent = $newContent -replace "(\r?\n){3,}", "`r`n`r`n"
    $newContent = $newContent.Trim()
    
    if ($newContent -ne $content) {
        Set-Content -Path $gitmodulesPath -Value $newContent -NoNewline
        Write-Green "Updated .gitmodules file"
        
        # 暂存更改到 Git
        $process = Start-Process -FilePath "git" -ArgumentList "add", ".gitmodules" -NoNewWindow -Wait -PassThru
        if ($process.ExitCode -eq 0) {
            Write-Green "  Staged .gitmodules changes"
        } else {
            Write-Red "  Warning: failed to stage .gitmodules changes"
        }
    } else {
        Write-Gray "No changes to .gitmodules file"
    }
}

Write-Cyan "======================================"
Write-Cyan "  Git Submodule Removal Tool"
Write-Cyan "======================================"
Write-Host ""

if (-not (Test-Path ".gitmodules")) {
    Write-Red "Error: .gitmodules not found. Are you in a git repository?"
    exit 1
}

if (-not (Test-Path ".git")) {
    Write-Red "Error: .git directory not found."
    exit 1
}

$submodules = Get-GitmodulesData

if ($submodules.Count -eq 0) {
    Write-Yellow "No submodules found in .gitmodules"
    exit 0
}

Write-Host "Total submodules in .gitmodules: $($submodules.Count)" -ForegroundColor Gray

$matchingSubmodules = Get-MatchingSubmodules -Submodules $submodules -Prefix $Prefix -PathPrefix $PathPrefix

if ($matchingSubmodules.Count -eq 0) {
    Write-Yellow "No submodules match the specified prefix(es)"
    if ($Prefix) { Write-Gray "  Prefix: $Prefix" }
    if ($PathPrefix) { Write-Gray "  PathPrefix: $PathPrefix" }
    exit 0
}

Write-Host ""
Write-Host "Matching submodules ($($matchingSubmodules.Count)):" -ForegroundColor Yellow

foreach ($sub in $matchingSubmodules) {
    Write-Host "  • $($sub.Name)" -ForegroundColor White -NoNewline
    Write-Host " -> $($sub.Path)" -ForegroundColor Gray
}

if ($DryRun) {
    Write-Host ""
    Write-Cyan "=== DRY RUN MODE ==="
    Write-Host "No changes will be made." -ForegroundColor Gray
}

if (-not $Force -and -not $DryRun) {
    Write-Host ""
    $confirm = Read-Host "Remove $($matchingSubmodules.Count) submodule(s)? (Y/N)"
    if ($confirm -ne 'Y' -and $confirm -ne 'y') {
        Write-Gray "Cancelled."
        exit 0
    }
}

Write-Host ""
$successCount = 0
$failCount = 0
$removedSubmodules = @()  # 新增：用于存储成功移除的子模块

foreach ($sub in $matchingSubmodules) {
    $result = Remove-Submodule -Submodule $sub -DryRun:$DryRun
    if ($result) {
        $removedSubmodules += $sub  # 记录成功移除的子模块
        $successCount++
    } else {
        $failCount++
    }
}

# 更新 .gitmodules 文件（仅当实际移除了子模块且非干运行时）
if ($removedSubmodules.Count -gt 0 -and -not $DryRun) {
    Update-GitmodulesFile -RemovedSubmodules $removedSubmodules
}

Write-Host ""
if ($DryRun) {
    Write-Cyan "Dry run completed! $($successCount) would be removed."
} else {
    Write-Green "Completed! Success: $successCount, Failed: $failCount"
}

if ($successCount -gt 0 -and -not $DryRun) {
    Write-Host ""
    Write-Yellow "Next steps:"
    Write-Host "  git status" -ForegroundColor Gray
    Write-Host "  git diff --cached" -ForegroundColor Gray
    Write-Host "  git commit -m 'Remove submodules'" -ForegroundColor Gray
    Write-Host "  git push" -ForegroundColor Gray
}