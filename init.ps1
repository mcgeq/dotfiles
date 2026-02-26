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
        if ($line -match '^\s*\[submodule "(.+)"\]\s*$') {
            if ($current) { $submodules += [pscustomobject]$current }
            $current = [ordered]@{
                Name = $matches[1]
                Path = ""
                Url = ""
            }
        }
        elseif ($line -match '^\s*(\w+)\s*=\s*(.+)$') {
            if ($current) {
                $current[$matches[1]] = $matches[2].Trim()
            }
        }
    }

    if ($current) { $submodules += [pscustomobject]$current }
    return @($submodules)
}

function Get-MatchingSubmodules {
    param(
        [array]$Submodules,
        [string]$Prefix,
        [string]$PathPrefix,
        [switch]$All
    )

    if ($All) {
        return @($Submodules)
    }

    $filtered = $Submodules

    if ($Prefix) {
        $filtered = $filtered | Where-Object { $_.Name -like "$Prefix*" }
    }

    if ($PathPrefix) {
        $filtered = $filtered | Where-Object { $_.Path -like "$PathPrefix*" }
    }

    return @($filtered)
}

function Add-Submodule {
    param(
        [object]$Submodule,
        [switch]$DryRun
    )

    $name = $Submodule.Name
    $path = $Submodule.Path
    $url = $Submodule.Url

    if (-not $name -or -not $path -or -not $url) {
        Write-Red "  Invalid entry: name/path/url is missing"
        return $false
    }

    $cmdPreview = "git submodule add --name $name --force $url $path"

    if ($DryRun) {
        Write-Yellow "[DRY-RUN] Would run: $cmdPreview"
        return $true
    }

    Write-Yellow "Installing: $name"

    if (Test-Path -LiteralPath $path) {
        $item = Get-Item -LiteralPath $path -ErrorAction SilentlyContinue
        if ($item -and $item.PSIsContainer) {
            $children = @(Get-ChildItem -LiteralPath $path -Force -ErrorAction SilentlyContinue | Where-Object { $_.Name -ne '.git' })
            if ($children.Count -eq 0) {
                Write-Yellow "  Empty directory detected, removing before add: $path"
                Remove-Item -LiteralPath $path -Force -Recurse -Confirm:$false -ErrorAction SilentlyContinue
                if (Test-Path -LiteralPath $path) {
                    Write-Red "  ✗ Failed: $name (could not remove empty directory: $path)"
                    return $false
                }
            } else {
                Write-Gray "  Directory is not empty, keep it: $path"
            }
        } else {
            Write-Gray "  Existing path is not a directory, keep it: $path"
        }
    }

    Write-Gray "  Running: $cmdPreview"
    $output = & git submodule add --name $name --force $url $path 2>&1
    $exitCode = $LASTEXITCODE

    if ($exitCode -eq 0) {
        Write-Green "  ✓ Installed: $name"
        return $true
    }

    $errorText = ($output | Out-String).Trim()
    if ($errorText) {
        foreach ($line in ($errorText -split "`r?`n")) {
            Write-Gray "  git: $line"
        }
    }

    Write-Red "  ✗ Failed: $name (exit code: $exitCode)"
    return $false
}

Write-Cyan "======================================"
Write-Cyan "  Git Submodule Force Install Tool"
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

if (-not $All -and -not $Prefix -and -not $PathPrefix) {
    Write-Red "Error: specify at least one filter (-Prefix/-PathPrefix) or pass -All"
    exit 1
}

Write-Host "Total submodules in .gitmodules: $($submodules.Count)" -ForegroundColor Gray

$matchingSubmodules = Get-MatchingSubmodules -Submodules $submodules -Prefix $Prefix -PathPrefix $PathPrefix -All:$All
if ($matchingSubmodules.Count -eq 0) {
    Write-Yellow "No submodules match the specified filter(s)"
    if ($Prefix) { Write-Gray "  Prefix: $Prefix" }
    if ($PathPrefix) { Write-Gray "  PathPrefix: $PathPrefix" }
    if ($All) { Write-Gray "  All: true" }
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
    $confirm = Read-Host "Install $($matchingSubmodules.Count) submodule(s) with --force? (Y/N)"
    if ($confirm -ne 'Y' -and $confirm -ne 'y') {
        Write-Gray "Cancelled."
        exit 0
    }
}

Write-Host ""
$successCount = 0
$failCount = 0

foreach ($sub in $matchingSubmodules) {
    $result = Add-Submodule -Submodule $sub -DryRun:$DryRun
    if ($result) {
        $successCount++
    } else {
        $failCount++
    }
}

Write-Host ""
if ($DryRun) {
    Write-Cyan "Dry run completed! $($successCount) would be installed."
} else {
    Write-Green "Completed! Success: $successCount, Failed: $failCount"
}

if ($successCount -gt 0 -and -not $DryRun) {
    Write-Host ""
    Write-Yellow "Next steps:"
    Write-Host "  git status" -ForegroundColor Gray
    Write-Host "  git diff --cached" -ForegroundColor Gray
    Write-Host "  git commit -m 'Add submodules'" -ForegroundColor Gray
    Write-Host "  git push" -ForegroundColor Gray
}
