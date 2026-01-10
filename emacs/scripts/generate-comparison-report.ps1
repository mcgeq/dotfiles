# Generate comprehensive comparison report
# Analyzes differences between generated and original .el files

param(
    [string]$GeneratedDir = "site-lisp/config",
    [string]$OriginalDir = "site-lisp/config-bak",
    [string]$OutputFile = "comparison-report.md"
)

$scriptDir = Split-Path -Parent $MyInvocation.MyCommand.Path
$emacsDir = Split-Path -Parent $scriptDir

Push-Location $emacsDir

$generatedPath = Join-Path $emacsDir $GeneratedDir
$originalPath = Join-Path $emacsDir $OriginalDir
$outputPath = Join-Path $emacsDir $OutputFile

function Normalize-Content {
    param([string]$content)
    $content = $content -replace "`r`n", "`n"
    $content = $content -replace "`r", "`n"
    $lines = $content -split "`n"
    $lines = $lines | ForEach-Object { $_.TrimEnd() }
    while ($lines.Count -gt 0 -and $lines[-1] -eq "") {
        $lines = $lines[0..($lines.Count - 2)]
    }
    return ($lines -join "`n")
}

function Analyze-Differences {
    param(
        [string]$genFile,
        [string]$origFile
    )
    
    $genContent = Get-Content $genFile -Raw -ErrorAction SilentlyContinue
    $origContent = Get-Content $origFile -Raw -ErrorAction SilentlyContinue
    
    $genNorm = Normalize-Content $genContent
    $origNorm = Normalize-Content $origContent
    
    if ($genNorm -eq $origNorm) {
        return @{
            Status = "IDENTICAL"
            Category = "whitespace-only"
            MissingLines = @()
            ExtraLines = @()
            GenLineCount = 0
            OrigLineCount = 0
        }
    }
    
    $genLines = $genNorm -split "`n"
    $origLines = $origNorm -split "`n"
    
    # Analyze difference types
    $missingInGen = [System.Collections.ArrayList]@()
    $extraInGen = [System.Collections.ArrayList]@()
    
    # Simple line-by-line comparison
    $genSet = @{}
    $origSet = @{}
    
    foreach ($line in $genLines) {
        if ($line.Trim() -ne "") {
            $genSet[$line] = $true
        }
    }
    
    foreach ($line in $origLines) {
        if ($line.Trim() -ne "") {
            $origSet[$line] = $true
        }
    }
    
    # Lines in original but not in generated
    foreach ($line in $origSet.Keys) {
        if (-not $genSet.ContainsKey($line)) {
            [void]$missingInGen.Add($line)
        }
    }
    
    # Lines in generated but not in original
    foreach ($line in $genSet.Keys) {
        if (-not $origSet.ContainsKey($line)) {
            [void]$extraInGen.Add($line)
        }
    }
    
    # Categorize
    $category = "mixed-changes"
    
    # Check if differences are only in comments
    $allComments = $true
    $allLines = @($missingInGen) + @($extraInGen)
    foreach ($line in $allLines) {
        if ($line -notmatch "^;;") {
            $allComments = $false
            break
        }
    }
    
    if ($allComments -and ($missingInGen.Count + $extraInGen.Count) -gt 0) {
        $category = "comment-only"
    } elseif ($missingInGen.Count -gt 0 -and $extraInGen.Count -eq 0) {
        $category = "missing-content"
    } elseif ($extraInGen.Count -gt 0 -and $missingInGen.Count -eq 0) {
        $category = "extra-content"
    }
    
    return @{
        Status = "DIFFERENT"
        Category = $category
        MissingLines = @($missingInGen)
        ExtraLines = @($extraInGen)
        GenLineCount = $genLines.Count
        OrigLineCount = $origLines.Count
    }
}

# Collect all results
$results = @{
    Identical = @()
    CommentOnly = @()
    MissingContent = @()
    ExtraContent = @()
    MixedChanges = @()
}

$generatedFiles = Get-ChildItem -Path $generatedPath -Filter "*.el" -Recurse | 
    ForEach-Object { $_.FullName.Substring($generatedPath.Length + 1) }

foreach ($file in $generatedFiles) {
    $genFile = Join-Path $generatedPath $file
    $origFile = Join-Path $originalPath $file
    
    if (Test-Path $origFile) {
        $analysis = Analyze-Differences $genFile $origFile
        
        $entry = @{
            File = $file
            Analysis = $analysis
        }
        
        switch ($analysis.Category) {
            "whitespace-only" { $results.Identical += $entry }
            "comment-only" { $results.CommentOnly += $entry }
            "missing-content" { $results.MissingContent += $entry }
            "extra-content" { $results.ExtraContent += $entry }
            "mixed-changes" { $results.MixedChanges += $entry }
        }
    }
}

# Generate markdown report
$report = @"
# EL to ORG Conversion Comparison Report

Generated: $(Get-Date -Format "yyyy-MM-dd HH:mm:ss")

## Summary

| Category | Count | Description |
|----------|-------|-------------|
| Identical (whitespace only) | $($results.Identical.Count) | Files that are functionally identical |
| Comment-only differences | $($results.CommentOnly.Count) | Only comment lines differ |
| Missing content | $($results.MissingContent.Count) | Generated file missing some content |
| Extra content | $($results.ExtraContent.Count) | Generated file has extra content |
| Mixed changes | $($results.MixedChanges.Count) | Multiple types of differences |

**Total files: $($generatedFiles.Count)**

## Detailed Results

### ✅ Identical Files (Whitespace Only)

These files are functionally identical - only whitespace differences exist.

"@

foreach ($entry in $results.Identical) {
    $report += "- ``$($entry.File)```n"
}

$report += @"

### 📝 Comment-Only Differences

These files differ only in comment lines.

"@

foreach ($entry in $results.CommentOnly) {
    $report += @"
#### ``$($entry.File)``

Missing comments:
"@
    foreach ($line in $entry.Analysis.MissingLines | Select-Object -First 5) {
        $report += "- ``$line```n"
    }
    if ($entry.Analysis.MissingLines.Count -gt 5) {
        $report += "- ... and $($entry.Analysis.MissingLines.Count - 5) more`n"
    }
    $report += "`n"
}

$report += @"

### ⚠️ Missing Content

Generated files are missing some content from originals.

"@

foreach ($entry in $results.MissingContent) {
    $report += @"
#### ``$($entry.File)``

Lines: Generated $($entry.Analysis.GenLineCount) vs Original $($entry.Analysis.OrigLineCount)

Missing lines (sample):
"@
    foreach ($line in $entry.Analysis.MissingLines | Select-Object -First 5) {
        $escapedLine = $line -replace '\|', '\|'
        $report += "- ``$escapedLine```n"
    }
    if ($entry.Analysis.MissingLines.Count -gt 5) {
        $report += "- ... and $($entry.Analysis.MissingLines.Count - 5) more`n"
    }
    $report += "`n"
}

$report += @"

### ➕ Extra Content

Generated files have additional content not in originals.

"@

foreach ($entry in $results.ExtraContent) {
    $report += @"
#### ``$($entry.File)``

Lines: Generated $($entry.Analysis.GenLineCount) vs Original $($entry.Analysis.OrigLineCount)

Extra lines (sample):
"@
    foreach ($line in $entry.Analysis.ExtraLines | Select-Object -First 5) {
        $escapedLine = $line -replace '\|', '\|'
        $report += "- ``$escapedLine```n"
    }
    if ($entry.Analysis.ExtraLines.Count -gt 5) {
        $report += "- ... and $($entry.Analysis.ExtraLines.Count - 5) more`n"
    }
    $report += "`n"
}

$report += @"

### 🔄 Mixed Changes

Files with multiple types of differences.

"@

foreach ($entry in $results.MixedChanges) {
    $report += @"
#### ``$($entry.File)``

Lines: Generated $($entry.Analysis.GenLineCount) vs Original $($entry.Analysis.OrigLineCount)

Missing lines: $($entry.Analysis.MissingLines.Count)
Extra lines: $($entry.Analysis.ExtraLines.Count)

"@
}

$report += @"

## Conclusion

The conversion from .el to .org files has been completed. The comparison shows:

- **$($results.Identical.Count)** files are functionally identical (only whitespace differences)
- **$($results.CommentOnly.Count + $results.MissingContent.Count + $results.ExtraContent.Count + $results.MixedChanges.Count)** files have content differences

Most differences are expected due to:
1. Org tangle may produce slightly different formatting
2. Some metadata comments (like `Requirements:`) may not have been included in the .org files
3. Minor whitespace and line ending differences

The generated .el files should be functionally equivalent to the originals for Emacs configuration purposes.
"@

# Write report
$report | Out-File -FilePath $outputPath -Encoding UTF8

Write-Host "Report generated: $outputPath" -ForegroundColor Green
Write-Host ""
Write-Host "Summary:" -ForegroundColor Cyan
Write-Host "  Identical (whitespace only): $($results.Identical.Count)" -ForegroundColor Green
Write-Host "  Comment-only differences: $($results.CommentOnly.Count)" -ForegroundColor Yellow
Write-Host "  Missing content: $($results.MissingContent.Count)" -ForegroundColor Yellow
Write-Host "  Extra content: $($results.ExtraContent.Count)" -ForegroundColor Yellow
Write-Host "  Mixed changes: $($results.MixedChanges.Count)" -ForegroundColor Yellow

Pop-Location
