#!/usr/bin/env pwsh
# restore-packages.ps1 - Restore to Locked Versions

param(
    [switch]$Force
)

$ScriptDir = Split-Path -Parent $MyInvocation.MyCommand.Path
$RootDir = Split-Path -Parent (Split-Path -Parent $ScriptDir)
Push-Location $RootDir

$lockFile = "emacs/site-lisp/packages-lock.el"

if (-not (Test-Path $lockFile)) {
    Write-Host ""
    Write-Host "[X] Lock file not found: $lockFile" -ForegroundColor Red
    Write-Host ""
    Write-Host "Tip: Run 'make lock-packages' first to create lock file" -ForegroundColor Yellow
    Write-Host ""
    Pop-Location
    exit 1
}

Write-Host ""
Write-Host "===============================================================" -ForegroundColor Cyan
Write-Host "  Restore to Locked Versions" -ForegroundColor Cyan
Write-Host "===============================================================" -ForegroundColor Cyan
Write-Host ""

# Parse lock file
$content = Get-Content $lockFile -Raw
$matches = [regex]::Matches($content, '\("([^"]+)"\s+\.\s+"([^"]+)"\)')

if ($matches.Count -eq 0) {
    Write-Host "[X] Cannot parse lock file" -ForegroundColor Red
    Pop-Location
    exit 1
}

$total = $matches.Count
$restored = 0
$failed = 0
$skipped = 0

Write-Host "Found $total locked package versions" -ForegroundColor Cyan
Write-Host ""

if (-not $Force) {
    $confirm = Read-Host "Continue to restore? (y/N)"
    if ($confirm -ne 'y' -and $confirm -ne 'Y') {
        Write-Host "Operation cancelled" -ForegroundColor Yellow
        Pop-Location
        exit 0
    }
    Write-Host ""
}

foreach ($match in $matches) {
    $path = $match.Groups[1].Value
    $commit = $match.Groups[2].Value
    
    $fullPath = "emacs/site-lisp/extensions/$path"
    
    if (-not (Test-Path $fullPath)) {
        Write-Host "[!] $path" -ForegroundColor Yellow -NoNewline
        Write-Host " (path not found)" -ForegroundColor Gray
        $skipped++
        continue
    }
    
    Push-Location $fullPath
    
    # Check current commit
    $currentCommit = git rev-parse HEAD 2>$null
    
    if ($currentCommit -eq $commit) {
        Write-Host "[OK] $path" -ForegroundColor Green -NoNewline
        Write-Host " (already at target)" -ForegroundColor Gray
        $skipped++
    }
    else {
        # Check for uncommitted changes
        $status = git status --porcelain 2>$null
        if ($status) {
            Write-Host "[!] $path" -ForegroundColor Yellow -NoNewline
            Write-Host " (local changes, skipped)" -ForegroundColor Gray
            $skipped++
        }
        else {
            # Checkout to locked commit
            git checkout $commit 2>&1 | Out-Null
            
            if ($LASTEXITCODE -eq 0) {
                Write-Host "[OK] $path" -ForegroundColor Green -NoNewline
                Write-Host " -> $($commit.Substring(0,7))" -ForegroundColor Cyan
                $restored++
            }
            else {
                Write-Host "[X] $path" -ForegroundColor Red -NoNewline
                Write-Host " (restore failed)" -ForegroundColor Gray
                $failed++
            }
        }
    }
    
    Pop-Location
}

Write-Host ""
Write-Host "===============================================================" -ForegroundColor Cyan
Write-Host "  Restore Complete" -ForegroundColor Cyan
Write-Host "===============================================================" -ForegroundColor Cyan
Write-Host ""
Write-Host "Total:       $total packages"
Write-Host "[OK] Restored: $restored" -ForegroundColor Green
Write-Host "[!]  Skipped:  $skipped" -ForegroundColor Yellow
Write-Host "[X]  Failed:   $failed" -ForegroundColor Red
Write-Host ""

if ($failed -gt 0) {
    Write-Host "[!] Some packages failed to restore, please check errors" -ForegroundColor Yellow
    Write-Host ""
}

Pop-Location
