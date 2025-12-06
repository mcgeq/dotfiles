#!/usr/bin/env pwsh
# Update-PackagesSafely.ps1 - Safely update packages with branch checking

$ScriptDir = Split-Path -Parent $MyInvocation.MyCommand.Path
$RootDir = Split-Path -Parent (Split-Path -Parent $ScriptDir)
Push-Location $RootDir

Write-Host ""
Write-Host "===============================================================" -ForegroundColor Cyan
Write-Host "  Safe Package Update" -ForegroundColor Cyan
Write-Host "===============================================================" -ForegroundColor Cyan
Write-Host ""

# Get all Emacs submodules
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
$switchedBranch = 0
$failed = 0
$skipped = 0

# Main branch names to try (in order)
$mainBranchNames = @('main', 'master', 'trunk', 'develop')

foreach ($line in $submodules) {
    $parts = $line -split "`t"
    $name = $parts[0]
    $path = $parts[1]
    
    if (-not (Test-Path $path)) {
        Write-Host "[-] $name (not initialized, skipped)" -ForegroundColor Yellow
        $skipped++
        continue
    }
    
    $total++
    $packageName = $name -replace "^emacs-", ""
    if (-not $packageName) {
        $packageName = Split-Path $path -Leaf
    }
    
    Push-Location $path
    
    # Get current branch
    $currentBranch = git rev-parse --abbrev-ref HEAD 2>$null
    
    # Check if there are uncommitted changes
    $status = git status --porcelain 2>$null
    if ($status) {
        Write-Host "[!] $packageName" -ForegroundColor Yellow -NoNewline
        Write-Host " (local changes, skipped)" -ForegroundColor Gray
        $skipped++
        Pop-Location
        continue
    }
    
    # Check if on a main branch
    $isOnMainBranch = $mainBranchNames -contains $currentBranch
    
    if (-not $isOnMainBranch) {
        # Try to switch to a main branch
        $foundMainBranch = $false
        foreach ($branchName in $mainBranchNames) {
            # Check if branch exists remotely
            $remoteBranch = git ls-remote --heads origin $branchName 2>$null
            if ($remoteBranch) {
                Write-Host "[*] $packageName" -ForegroundColor Cyan -NoNewline
                Write-Host " (switching $currentBranch -> $branchName)" -ForegroundColor Gray
                
                git checkout $branchName 2>&1 | Out-Null
                if ($LASTEXITCODE -eq 0) {
                    $switchedBranch++
                    $foundMainBranch = $true
                    $currentBranch = $branchName
                    break
                }
            }
        }
        
        if (-not $foundMainBranch) {
            Write-Host "[!] $packageName" -ForegroundColor Yellow -NoNewline
            Write-Host " (on '$currentBranch', no main branch found)" -ForegroundColor Gray
        }
    }
    
    # Update the package
    Write-Host "[>>] $packageName" -ForegroundColor White -NoNewline
    Write-Host " (updating on '$currentBranch')" -ForegroundColor DarkGray
    
    git pull --ff-only 2>&1 | Out-Null
    
    if ($LASTEXITCODE -eq 0) {
        Write-Host "     [OK] Updated" -ForegroundColor Green
        $updated++
    } else {
        Write-Host "     [X] Update failed" -ForegroundColor Red
        $failed++
    }
    
    Pop-Location
}

Write-Host ""
Write-Host "===============================================================" -ForegroundColor Cyan
Write-Host "  Update Summary" -ForegroundColor Cyan
Write-Host "===============================================================" -ForegroundColor Cyan
Write-Host ""
Write-Host "Total:             $total packages"
Write-Host "[OK] Updated:       $updated" -ForegroundColor Green
Write-Host "[*]  Switched branch: $switchedBranch" -ForegroundColor Cyan
Write-Host "[!]  Skipped:       $skipped" -ForegroundColor Yellow
Write-Host "[X]  Failed:        $failed" -ForegroundColor Red
Write-Host ""

if ($switchedBranch -gt 0) {
    Write-Host "Note: $switchedBranch package(s) were switched to main branch" -ForegroundColor Cyan
}

if ($failed -gt 0) {
    Write-Host "[!] Some packages failed to update, please check errors above" -ForegroundColor Yellow
}

Write-Host ""

Pop-Location
