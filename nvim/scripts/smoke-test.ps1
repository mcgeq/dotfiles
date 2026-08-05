param(
    [string]$Nvim = "nvim"
)

$root = Split-Path -Parent $PSScriptRoot
& $Nvim --headless -u NONE -l (Join-Path $root "tests\smoke.lua")
exit $LASTEXITCODE
