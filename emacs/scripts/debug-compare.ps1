# Debug comparison script
$genPath = "site-lisp\config"
$origPath = "site-lisp\config-bak"

$emacsDir = Split-Path -Parent (Split-Path -Parent $MyInvocation.MyCommand.Path)
Push-Location $emacsDir

$genFullPath = Join-Path $emacsDir $genPath
$origFullPath = Join-Path $emacsDir $origPath

Write-Host "Emacs dir: $emacsDir"
Write-Host "Gen path: $genFullPath"
Write-Host "Orig path: $origFullPath"
Write-Host "Gen exists: $(Test-Path $genFullPath)"
Write-Host "Orig exists: $(Test-Path $origFullPath)"

$files = Get-ChildItem -Path $genFullPath -Filter "*.el" -Recurse
Write-Host "Found $($files.Count) files"

foreach ($f in $files | Select-Object -First 3) {
    $rel = $f.FullName.Substring($genFullPath.Length + 1)
    Write-Host ""
    Write-Host "File: $rel"
    
    # Use -replace to handle path separators
    $origFile = "$origFullPath\$rel"
    Write-Host "  Orig file: $origFile"
    Write-Host "  Orig exists: $(Test-Path $origFile)"
    
    if (Test-Path $origFile) {
        $genContent = Get-Content $f.FullName -Raw
        $origContent = Get-Content $origFile -Raw
        
        Write-Host "  Gen length: $($genContent.Length)"
        Write-Host "  Orig length: $($origContent.Length)"
        Write-Host "  Equal: $($genContent -eq $origContent)"
    }
}

Pop-Location
