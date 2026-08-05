param(
    [string]$Nvim = "nvim",
    [int]$Runs = 3,
    [switch]$NoLoader
)

$root = Split-Path -Parent $PSScriptRoot
$tmp = Join-Path $env:TEMP "nvim-startuptime"
if (-not (Test-Path $tmp)) { New-Item -ItemType Directory -Path $tmp | Out-Null }

$init = Join-Path $root "init.lua"
if ($NoLoader) {
    $init = Join-Path $tmp "init-noloader.lua"
    $content = Get-Content (Join-Path $root "init.lua") -Raw
    $content = $content -replace "^\s*vim\.loader\.enable\(\)\s*$", ""
    Set-Content -Path $init -Value $content -Encoding utf8
}

$totals = @()
$samples = @{}

for ($i = 1; $i -le $Runs; $i++) {
    $file = Join-Path $tmp "start-$i.txt"
    & $Nvim --headless -u $init "--startuptime" $file -c "qa" | Out-Null
    if (-not (Test-Path $file)) {
        Write-Error "No startuptime file produced for run $i"
        exit 1
    }

    $lastClock = 0.0
    Get-Content $file | ForEach-Object {
        if ($_ -match '^\s*(\d+\.\d+)\s+(\d+\.\d+)\s+(\d+\.\d+)\s+(.*)$') {
            $lastClock = [double]$matches[1]
            $key = $matches[4].Trim()
            if ($key -and -not $samples.ContainsKey($key)) { $samples[$key] = @() }
            if ($key) { $samples[$key] += [double]$matches[3] }
        } elseif ($_ -match '^\s*(\d+\.\d+)\s+(\d+\.\d+)\s*:\s*(.*)$') {
            $lastClock = [double]$matches[1]
            $key = $matches[3].Trim()
            if ($key -and -not $samples.ContainsKey($key)) { $samples[$key] = @() }
            if ($key) { $samples[$key] += [double]$matches[2] }
        }
    }
    $totals += $lastClock
}

"runs (ms): " + (($totals | ForEach-Object { "{0:N1}" -f $_ }) -join ", ")
$median = $totals | Sort-Object | Select-Object -Skip ([math]::Floor(($totals.Count - 1) / 2)) -First 1
"median: {0:N1} ms" -f $median

"`ntop 10 slowest items (self time, median across runs):"
$items = @()
foreach ($k in $samples.Keys) {
    $sorted = @($samples[$k] | Sort-Object)
    $m = $sorted[[math]::Floor(($sorted.Count - 1) / 2)]
    $items += [PSCustomObject]@{ Key = $k; Median = $m }
}
$items | Sort-Object Median -Descending | Select-Object -First 10 | ForEach-Object {
    "{0,8:N1} ms  {1}" -f $_.Median, $_.Key
}
