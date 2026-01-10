# Detailed diff comparison for .el files
# Shows actual differences between generated and original files

param(
    [string]$GeneratedDir = "site-lisp/config",
    [string]$OriginalDir = "site-lisp/config-bak",
    [string]$File = "",
    [switch]$ShowAll,
    [switch]$IgnoreWhitespace
)

$scriptDir = Split-Path -Parent $MyInvocation.MyCommand.Path
$emacsDir = Split-Path -Parent $scriptDir

Push-Location $emacsDir

$generatedPath = Join-Path $emacsDir $GeneratedDir
$originalPath = Join-Path $emacsDir $OriginalDir

function Normalize-Content {
    param([string]$content)
    
    # Normalize line endings
    $content = $content -replace "`r`n", "`n"
    $content = $content -replace "`r", "`n"
    
    # Remove trailing whitespace from each line
    $lines = $content -split "`n"
    $lines = $lines | ForEach-Object { $_.TrimEnd() }
    
    # Remove trailing empty lines
    while ($lines.Count -gt 0 -and $lines[-1] -eq "") {
        $lines = $lines[0..($lines.Count - 2)]
    }
    
    return ($lines -join "`n")
}

function Compare-Files {
    param(
        [string]$genFile,
        [string]$origFile,
        [string]$relativePath
    )
    
    $genContent = Get-Content $genFile -Raw -ErrorAction SilentlyContinue
    $origContent = Get-Content $origFile -Raw -ErrorAction SilentlyContinue
    
    # Normalize content
    $genNorm = Normalize-Content $genContent
    $origNorm = Normalize-Content $origContent
    
    if ($genNorm -eq $origNorm) {
        return @{
            Status = "IDENTICAL"
            File = $relativePath
            Details = "Files are functionally identical (whitespace differences only)"
        }
    }
    
    # Split into lines for detailed comparison
    $genLines = $genNorm -split "`n"
    $origLines = $origNorm -split "`n"
    
    $differences = @()
    $maxLines = [Math]::Max($genLines.Count, $origLines.Count)
    
    for ($i = 0; $i -lt $maxLines; $i++) {
        $genLine = if ($i -lt $genLines.Count) { $genLines[$i] } else { $null }
        $origLine = if ($i -lt $origLines.Count) { $origLines[$i] } else { $null }
        
        if ($genLine -ne $origLine) {
            $differences += @{
                Line = $i + 1
                Generated = $genLine
                Original = $origLine
            }
        }
    }
    
    return @{
        Status = "DIFFERENT"
        File = $relativePath
        GenLines = $genLines.Count
        OrigLines = $origLines.Count
        DiffCount = $differences.Count
        Differences = $differences
    }
}

if ($File -ne "") {
    # Compare single file
    $genFile = Join-Path $generatedPath $File
    $origFile = Join-Path $originalPath $File
    
    if (-not (Test-Path $genFile)) {
        Write-Host "Generated file not found: $genFile" -ForegroundColor Red
        exit 1
    }
    if (-not (Test-Path $origFile)) {
        Write-Host "Original file not found: $origFile" -ForegroundColor Red
        exit 1
    }
    
    $result = Compare-Files $genFile $origFile $File
    
    Write-Host "============================================" -ForegroundColor Cyan
    Write-Host "Comparing: $File" -ForegroundColor Cyan
    Write-Host "============================================" -ForegroundColor Cyan
    Write-Host ""
    
    if ($result.Status -eq "IDENTICAL") {
        Write-Host "[IDENTICAL] $($result.Details)" -ForegroundColor Green
    } else {
        Write-Host "[DIFFERENT] Generated: $($result.GenLines) lines, Original: $($result.OrigLines) lines" -ForegroundColor Yellow
        Write-Host "            $($result.DiffCount) line differences found" -ForegroundColor Yellow
        Write-Host ""
        
        $showCount = if ($ShowAll) { $result.Differences.Count } else { [Math]::Min(10, $result.Differences.Count) }
        
        for ($i = 0; $i -lt $showCount; $i++) {
            $diff = $result.Differences[$i]
            Write-Host "Line $($diff.Line):" -ForegroundColor White
            if ($null -ne $diff.Original) {
                Write-Host "  - $($diff.Original)" -ForegroundColor Red
            } else {
                Write-Host "  - (line not in original)" -ForegroundColor Red
            }
            if ($null -ne $diff.Generated) {
                Write-Host "  + $($diff.Generated)" -ForegroundColor Green
            } else {
                Write-Host "  + (line not in generated)" -ForegroundColor Green
            }
            Write-Host ""
        }
        
        if (-not $ShowAll -and $result.Differences.Count -gt 10) {
            Write-Host "... and $($result.Differences.Count - 10) more differences (use -ShowAll to see all)" -ForegroundColor Gray
        }
    }
} else {
    # Compare all files
    Write-Host "============================================" -ForegroundColor Cyan
    Write-Host "Detailed Comparison Report" -ForegroundColor Cyan
    Write-Host "============================================" -ForegroundColor Cyan
    Write-Host ""
    
    $generatedFiles = Get-ChildItem -Path $generatedPath -Filter "*.el" -Recurse | 
        ForEach-Object { $_.FullName.Substring($generatedPath.Length + 1) }
    
    $identical = @()
    $different = @()
    
    foreach ($file in $generatedFiles) {
        $genFile = Join-Path $generatedPath $file
        $origFile = Join-Path $originalPath $file
        
        if (Test-Path $origFile) {
            $result = Compare-Files $genFile $origFile $file
            
            if ($result.Status -eq "IDENTICAL") {
                $identical += $file
            } else {
                $different += $result
            }
        }
    }
    
    Write-Host "FUNCTIONALLY IDENTICAL (whitespace only): $($identical.Count)" -ForegroundColor Green
    foreach ($file in $identical) {
        Write-Host "  [OK] $file" -ForegroundColor DarkGreen
    }
    Write-Host ""
    
    Write-Host "CONTENT DIFFERENCES: $($different.Count)" -ForegroundColor Yellow
    foreach ($result in $different) {
        Write-Host "  [DIFF] $($result.File) - $($result.DiffCount) differences" -ForegroundColor Yellow
    }
    Write-Host ""
    
    # Summary
    $total = $identical.Count + $different.Count
    Write-Host "============================================" -ForegroundColor Cyan
    Write-Host "SUMMARY" -ForegroundColor Cyan
    Write-Host "============================================" -ForegroundColor Cyan
    Write-Host "Total: $total files"
    Write-Host "Functionally Identical: $($identical.Count) ($([math]::Round($identical.Count / $total * 100, 1))%)"
    Write-Host "Content Differences: $($different.Count) ($([math]::Round($different.Count / $total * 100, 1))%)"
}

Pop-Location
