#!/usr/bin/env pwsh
# lock-packages.ps1 - Lock Git Submodules Versions

$ScriptDir = Split-Path -Parent $MyInvocation.MyCommand.Path
$RootDir = Split-Path -Parent (Split-Path -Parent $ScriptDir)
Push-Location $RootDir

Write-Host ""
Write-Host "Locking package versions..." -ForegroundColor Cyan
Write-Host ""

# Output file
$outputFile = "emacs/site-lisp/packages-lock.el"

# Generate file header
$output = @"
;;; packages-lock.el --- Locked package versions -*- lexical-binding: t; -*-

;; Filename: packages-lock.el
;; Description: Locked extension package versions
;; Auto-generated: $(Get-Date -Format "yyyy-MM-dd HH:mm:ss")

;;; Commentary:
;;
;; This file is auto-generated, recording all Git Submodules commit hashes
;; Used for version control and rollback
;;

;;; Code:

(defvar mcg-packages-lock
  '(
"@

# Get all Emacs submodules
$packages = @()
$count = 0

Get-ChildItem -Path "emacs/site-lisp/extensions" -Directory -Recurse |
    Where-Object { Test-Path "$($_.FullName)/.git" } |
    ForEach-Object {
        Push-Location $_.FullName
        
        $commit = git rev-parse HEAD 2>$null
        
        if ($commit) {
            $relativePath = $_.FullName.Replace("$RootDir\emacs\site-lisp\extensions\", "").Replace("\", "/")
            $packages += "    (`"$relativePath`" . `"$commit`")"
            $count++
            Write-Host "[OK] $relativePath" -ForegroundColor Green -NoNewline
            Write-Host " -> $($commit.Substring(0,7))" -ForegroundColor Gray
        }
        
        Pop-Location
    }

$output += $packages -join "`n"

$output += @"

  )
  "Locked package versions and their commit hashes")

(provide 'mcg-packages-lock)
;;; packages-lock.el ends here
"@

# Write to file
Set-Content -Path $outputFile -Value $output -Encoding UTF8

Write-Host ""
Write-Host "[OK] Locked $count package versions" -ForegroundColor Green
Write-Host "Output: $outputFile" -ForegroundColor Cyan
Write-Host ""
Write-Host "Tip: Commit this file to Git for team consistency" -ForegroundColor Yellow
Write-Host ""

Pop-Location
