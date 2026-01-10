# Check parentheses matching in Emacs Lisp code blocks within .org files

param(
    [string]$File
)

if (-not $File) {
    Write-Host "Usage: check-parens.ps1 -File <path-to-org-file>" -ForegroundColor Red
    exit 1
}

$scriptDir = Split-Path -Parent $MyInvocation.MyCommand.Path
$emacsDir = Split-Path -Parent $scriptDir

$filePath = Join-Path $emacsDir $File
if (-not (Test-Path $filePath)) {
    $filePath = $File
}

if (-not (Test-Path $filePath)) {
    Write-Host "File not found: $filePath" -ForegroundColor Red
    exit 1
}

Write-Host "Checking parentheses in: $filePath" -ForegroundColor Cyan
Write-Host ""

$content = Get-Content $filePath -Raw
$lines = Get-Content $filePath

# Extract all emacs-lisp code blocks
$inCodeBlock = $false
$codeBlocks = @()
$currentBlock = @{
    StartLine = 0
    EndLine = 0
    Content = ""
    Lines = @()
}

for ($i = 0; $i -lt $lines.Count; $i++) {
    $line = $lines[$i]
    
    if ($line -match '^\s*#\+begin_src\s+emacs-lisp') {
        $inCodeBlock = $true
        $currentBlock = @{
            StartLine = $i + 1
            EndLine = 0
            Content = ""
            Lines = @()
        }
    }
    elseif ($line -match '^\s*#\+end_src' -and $inCodeBlock) {
        $inCodeBlock = $false
        $currentBlock.EndLine = $i + 1
        $codeBlocks += $currentBlock
    }
    elseif ($inCodeBlock) {
        $currentBlock.Content += $line + "`n"
        $currentBlock.Lines += @{
            LineNum = $i + 1
            Text = $line
        }
    }
}

Write-Host "Found $($codeBlocks.Count) code blocks" -ForegroundColor Gray
Write-Host ""

$totalErrors = 0

foreach ($block in $codeBlocks) {
    $openParens = 0
    $openBrackets = 0
    $openBraces = 0
    $inString = $false
    $inComment = $false
    $errors = @()
    
    foreach ($lineInfo in $block.Lines) {
        $line = $lineInfo.Text
        $lineNum = $lineInfo.LineNum
        $inComment = $false
        
        for ($j = 0; $j -lt $line.Length; $j++) {
            $char = $line[$j]
            $prevChar = if ($j -gt 0) { $line[$j-1] } else { $null }
            
            # Skip escaped characters
            if ($prevChar -eq '\') {
                continue
            }
            
            # Handle strings
            if ($char -eq '"' -and -not $inComment) {
                $inString = -not $inString
                continue
            }
            
            # Handle comments (;)
            if ($char -eq ';' -and -not $inString) {
                $inComment = $true
                continue
            }
            
            if ($inString -or $inComment) {
                continue
            }
            
            # Count parentheses
            switch ($char) {
                '(' { $openParens++ }
                ')' { 
                    $openParens--
                    if ($openParens -lt 0) {
                        $errors += "Line $lineNum`: Extra closing ')' - $line"
                        $openParens = 0
                    }
                }
                '[' { $openBrackets++ }
                ']' { 
                    $openBrackets--
                    if ($openBrackets -lt 0) {
                        $errors += "Line $lineNum`: Extra closing ']' - $line"
                        $openBrackets = 0
                    }
                }
            }
        }
    }
    
    # Check for unclosed parentheses at end of block
    if ($openParens -ne 0) {
        $errors += "Block ending at line $($block.EndLine): $openParens unclosed '('"
    }
    if ($openBrackets -ne 0) {
        $errors += "Block ending at line $($block.EndLine): $openBrackets unclosed '['"
    }
    
    if ($errors.Count -gt 0) {
        Write-Host "Code block (lines $($block.StartLine)-$($block.EndLine)):" -ForegroundColor Yellow
        foreach ($err in $errors) {
            Write-Host "  [ERROR] $err" -ForegroundColor Red
            $totalErrors++
        }
        Write-Host ""
    }
}

Write-Host "========================================" -ForegroundColor Cyan
if ($totalErrors -eq 0) {
    Write-Host "[OK] No parentheses errors found!" -ForegroundColor Green
} else {
    Write-Host "[WARNING] Found $totalErrors potential issues" -ForegroundColor Yellow
}
