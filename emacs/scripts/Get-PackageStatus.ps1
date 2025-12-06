#!/usr/bin/env pwsh
# check-submodules.ps1 - Check Git Submodules Status

param(
    [switch]$Detailed
)

$ScriptDir = Split-Path -Parent $MyInvocation.MyCommand.Path
$RootDir = Split-Path -Parent (Split-Path -Parent $ScriptDir)
Push-Location $RootDir

Write-Host ""
Write-Host "===============================================================" -ForegroundColor Cyan
Write-Host "  Git Submodules Status Check" -ForegroundColor Cyan
Write-Host "===============================================================" -ForegroundColor Cyan
Write-Host ""

# 获取所有 Emacs submodules
$submodules = git config --file .gitmodules --get-regexp path | 
    Where-Object { $_ -match "emacs/site-lisp/extensions" } |
    ForEach-Object { 
        $_ -replace "submodule\.", "" -replace "\.path ", "`t"
    }

if (-not $submodules) {
    Write-Host "[X] No Emacs extension submodules found" -ForegroundColor Red
    Pop-Location
    exit 1
}

$total = 0
$updated = 0
$outdated = 0
$modified = 0
$uninitialized = 0

foreach ($line in $submodules) {
    $parts = $line -split "`t"
    $name = $parts[0]
    $path = $parts[1]
    
    if (-not (Test-Path $path)) {
        $uninitialized++
        Write-Host "[-] $name" -ForegroundColor Yellow -NoNewline
        Write-Host " (not initialized)" -ForegroundColor Gray
        continue
    }
    
    $total++
    
    Push-Location $path
    
    # 获取当前 commit
    $currentCommit = git rev-parse --short HEAD 2>$null
    
    # 检查是否有未提交的修改
    $status = git status --porcelain 2>$null
    $hasChanges = $status.Length -gt 0
    
    # 获取远程更新
    git fetch origin 2>$null | Out-Null
    $remoteCommit = git rev-parse --short origin/HEAD 2>$null
    if (-not $remoteCommit) {
        $remoteCommit = git rev-parse --short origin/master 2>$null
    }
    if (-not $remoteCommit) {
        $remoteCommit = git rev-parse --short origin/main 2>$null
    }
    
    $needsUpdate = $currentCommit -ne $remoteCommit
    
    # 获取包名（使用 submodule 名称或路径的最后部分）
    $packageName = $name -replace "^emacs-", ""
    if (-not $packageName) {
        $packageName = Split-Path $path -Leaf
    }
    
    if ($hasChanges) {
        $modified++
        Write-Host "[!]  $packageName" -ForegroundColor Yellow -NoNewline
        Write-Host " (local changes)" -ForegroundColor Gray
        if ($Detailed) {
            Write-Host "     Current: $currentCommit" -ForegroundColor DarkGray
            Write-Host "     $status" -ForegroundColor DarkGray
        }
    }
    elseif ($needsUpdate) {
        $outdated++
        Write-Host "[*]  $packageName" -ForegroundColor Cyan -NoNewline
        Write-Host " (update available)" -ForegroundColor Gray
        if ($Detailed) {
            Write-Host "     Current: $currentCommit -> Remote: $remoteCommit" -ForegroundColor DarkGray
        }
    }
    else {
        $updated++
        Write-Host "[OK] $packageName" -ForegroundColor Green
        if ($Detailed) {
            Write-Host "     ($currentCommit)" -ForegroundColor DarkGray
        }
    }
    
    Pop-Location
}

Write-Host ""
Write-Host "===============================================================" -ForegroundColor Cyan
Write-Host "  Summary" -ForegroundColor Cyan
Write-Host "===============================================================" -ForegroundColor Cyan
Write-Host ""
Write-Host "Total:           $($total + $uninitialized) packages"
Write-Host "[OK] Up-to-date:  $updated" -ForegroundColor Green
Write-Host "[*]  Outdated:    $outdated" -ForegroundColor Cyan
Write-Host "[!]  Modified:    $modified" -ForegroundColor Yellow
Write-Host "[-]  Uninitialized: $uninitialized" -ForegroundColor Yellow
Write-Host ""

if ($outdated -gt 0) {
    Write-Host "Tip: Run 'make up' to update all packages" -ForegroundColor Yellow
}

if ($uninitialized -gt 0) {
    Write-Host "Tip: Run 'git submodule update --init --recursive' to initialize" -ForegroundColor Yellow
}

Write-Host ""

Pop-Location
