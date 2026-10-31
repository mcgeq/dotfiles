param(
    [Parameter(Mandatory = $true)]
    [string]$PluginRoot,
    [string]$Nvim = "nvim"
)

$root = Split-Path -Parent $PSScriptRoot
$resolvedPlugins = (Resolve-Path -LiteralPath $PluginRoot -ErrorAction Stop).Path
$names = @('XDG_CONFIG_HOME', 'XDG_DATA_HOME', 'XDG_STATE_HOME', 'XDG_CACHE_HOME', 'NVIM_TEST_PACK_ROOT', 'NVIM_APPNAME', 'NVIM_NO_PLUGINS')
$saved = @{}
foreach ($name in $names) { $saved[$name] = [Environment]::GetEnvironmentVariable($name) }
$testRoot = Join-Path ([IO.Path]::GetTempPath()) ('nvim-maintenance-' + [guid]::NewGuid().ToString('N'))
$testExit = 1
try {
    $env:XDG_CONFIG_HOME = Split-Path -Parent $root
    $env:XDG_DATA_HOME = Join-Path $testRoot 'data'
    $env:XDG_STATE_HOME = Join-Path $testRoot 'state'
    $env:XDG_CACHE_HOME = Join-Path $testRoot 'cache'
    $env:NVIM_APPNAME = Split-Path -Leaf $root
    $env:NVIM_NO_PLUGINS = $null
    $env:NVIM_TEST_PACK_ROOT = $resolvedPlugins
    & $Nvim --headless -u NONE -i NONE -l (Join-Path $root 'tests/startup.lua')
    $testExit = $LASTEXITCODE
} finally {
    foreach ($name in $names) { [Environment]::SetEnvironmentVariable($name, $saved[$name]) }
}
exit $testExit
