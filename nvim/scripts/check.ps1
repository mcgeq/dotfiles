param(
    [string]$Nvim = "nvim",
    [int]$StartupThresholdMs = 400
)

$root = Split-Path -Parent $PSScriptRoot
$failed = $false

Write-Host "== selene (lint) =="
selene (Join-Path $root "lua")
if ($LASTEXITCODE -ne 0) { $failed = $true }

Write-Host "`n== stylua (format check) =="
stylua --check (Join-Path $root "lua") (Join-Path $root "tests")
if ($LASTEXITCODE -ne 0) { $failed = $true }

Write-Host "`n== smoke (module load) =="
& $Nvim --headless -u NONE -l (Join-Path $root "tests\smoke.lua")
if ($LASTEXITCODE -ne 0) { $failed = $true }

Write-Host "`n== servers (LSP registry) =="
& $Nvim --headless -u NONE -l (Join-Path $root "tests\verify_servers.lua")
if ($LASTEXITCODE -ne 0) { $failed = $true }

Write-Host "`n== startup (timing) =="
$startupOut = & (Join-Path $PSScriptRoot "startup.ps1") -Nvim $Nvim -Runs 2 2>$null
$startupOut | ForEach-Object { Write-Host "  $_" }
$medianLine = $startupOut | Where-Object { $_ -match '^median: (\d+\.\d+) ms' }
if ($medianLine) {
    $median = [double]($medianLine -replace '^median: (\d+\.\d+) ms', '$1')
    if ($median -gt $StartupThresholdMs) {
        Write-Host "  startup median {0:N1} ms exceeds threshold {1} ms" -f $median, $StartupThresholdMs -ForegroundColor Yellow
    }
} else {
    Write-Host "  could not parse startup timing" -ForegroundColor Yellow
}

if ($failed) {
    Write-Host "`nCHECK FAILED" -ForegroundColor Red
    exit 1
}
Write-Host "`nALL CHECKS PASSED" -ForegroundColor Green
