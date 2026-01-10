# Compare generated .el files with original backup files
# This script compares files in site-lisp/config with site-lisp/config-bak

param(
    [string]$GeneratedDir = "site-lisp/config",
    [string]$OriginalDir = "site-lisp/config-bak",
    [switch]$Detailed
)

$scriptDir = Split-Path -Parent $MyInvocation.MyCommand.Path
$emacsDir = Split-Path -Parent $scriptDir

Push-Location $emacsDir

$generatedPath = Join-Path $emacsDir $GeneratedDir
$originalPath = Join-Path $emacsDir $OriginalDir

Write-Host "============================================" -ForegroundColor Cyan
Write-Host "Comparing Generated vs Original .el Files" -ForegroundColor Cyan
Write-Host "============================================" -ForegroundColor Cyan
Write-Host ""
Write-Host "Generated: $generatedPath" -ForegroundColor Gray
Write-Host "Original:  $originalPath" -ForegroundColor Gray
Write-Host ""

# Get all .el files from both directories
$generatedFiles = Get-ChildItem -Path $generatedPath -Filter "*.el" -Recurse | 
    ForEach-Object { $_.FullName.Substring($generatedPath.Length + 1) }

$originalFiles = Get-ChildItem -Path $originalPath -Filter "*.el" -Recurse | 
    ForEach-Object { $_.FullName.Substring($originalPath.Length + 1) }

$results = @{
    Identical = @()
    Different = @()
    OnlyInGenerated = @()
    OnlyInOriginal = @()
}

# Compare files that exist in both directories
foreach ($file in $generatedFiles) {
    $genFile = Join-Path $generatedPath $file
    $origFile = Join-Path $originalPath $file
    
    if (Test-Path $origFile) {
        # Both files exist, compare content
        $genContent = Get-Content $genFile -Raw -ErrorAction SilentlyContinue
        $origContent = Get-Content $origFile -Raw -ErrorAction SilentlyContinue
        
        if ($genContent -eq $origContent) {
            $results.Identical += $file
        } else {
            $results.Different += @{
                File = $file
                GenLines = ($genContent -split "`n").Count
                OrigLines = ($origContent -split "`n").Count
            }
        }
    } else {
        $results.OnlyInGenerated += $file
    }
}

# Find files only in original
foreach ($file in $originalFiles) {
    if ($file -notin $generatedFiles) {
        $results.OnlyInOriginal += $file
    }
}

# Output results
Write-Host "============================================" -ForegroundColor Green
Write-Host "COMPARISON RESULTS" -ForegroundColor Green
Write-Host "============================================" -ForegroundColor Green
Write-Host ""

Write-Host "Identical Files: $($results.Identical.Count)" -ForegroundColor Green
if ($Detailed -and $results.Identical.Count -gt 0) {
    foreach ($file in $results.Identical) {
        Write-Host "  [OK] $file" -ForegroundColor DarkGreen
    }
}
Write-Host ""

Write-Host "Different Files: $($results.Different.Count)" -ForegroundColor Yellow
if ($results.Different.Count -gt 0) {
    foreach ($item in $results.Different) {
        Write-Host "  [DIFF] $($item.File)" -ForegroundColor Yellow
        Write-Host "         Generated: $($item.GenLines) lines, Original: $($item.OrigLines) lines" -ForegroundColor Gray
    }
}
Write-Host ""

Write-Host "Only in Generated: $($results.OnlyInGenerated.Count)" -ForegroundColor Cyan
if ($results.OnlyInGenerated.Count -gt 0) {
    foreach ($file in $results.OnlyInGenerated) {
        Write-Host "  [NEW] $file" -ForegroundColor Cyan
    }
}
Write-Host ""

Write-Host "Only in Original: $($results.OnlyInOriginal.Count)" -ForegroundColor Magenta
if ($results.OnlyInOriginal.Count -gt 0) {
    foreach ($file in $results.OnlyInOriginal) {
        Write-Host "  [MISSING] $file" -ForegroundColor Magenta
    }
}
Write-Host ""

# Summary
Write-Host "============================================" -ForegroundColor Cyan
Write-Host "SUMMARY" -ForegroundColor Cyan
Write-Host "============================================" -ForegroundColor Cyan
$total = $results.Identical.Count + $results.Different.Count
Write-Host "Total files compared: $total"
Write-Host "Identical: $($results.Identical.Count) ($([math]::Round($results.Identical.Count / $total * 100, 1))%)"
Write-Host "Different: $($results.Different.Count) ($([math]::Round($results.Different.Count / $total * 100, 1))%)"

if ($results.Different.Count -eq 0 -and $results.OnlyInOriginal.Count -eq 0) {
    Write-Host ""
    Write-Host "[SUCCESS] All generated files match the originals!" -ForegroundColor Green
} else {
    Write-Host ""
    Write-Host "[WARNING] Some files differ or are missing. Review the differences above." -ForegroundColor Yellow
}

Pop-Location

# Return exit code based on results
if ($results.Different.Count -eq 0 -and $results.OnlyInOriginal.Count -eq 0) {
    exit 0
} else {
    exit 1
}
