$ErrorActionPreference = 'Stop'

$scriptDir = Split-Path -Parent $MyInvocation.MyCommand.Path
$repoRoot = (Resolve-Path (Join-Path $scriptDir '..\..')).Path
$docPath = Join-Path $repoRoot 'vimrc/docs/keymaps.md'

function Test-ByteSequence {
  param(
    [byte[]]$Data,
    [byte[]]$Pattern
  )

  if ($Pattern.Length -eq 0 -or $Data.Length -lt $Pattern.Length) {
    return $false
  }

  for ($i = 0; $i -le $Data.Length - $Pattern.Length; $i++) {
    $matched = $true
    for ($j = 0; $j -lt $Pattern.Length; $j++) {
      if ($Data[$i + $j] -ne $Pattern[$j]) {
        $matched = $false
        break
      }
    }

    if ($matched) {
      return $true
    }
  }

  return $false
}

function Invoke-VimCommand {
  param(
    [string[]]$Arguments,
    [int]$TimeoutMs = 30000
  )

  $process = Start-Process -FilePath 'vim' -ArgumentList $Arguments -PassThru -NoNewWindow
  if (-not $process.WaitForExit($TimeoutMs)) {
    $process | Stop-Process -Force
    throw "vim timed out after $TimeoutMs ms"
  }

  if ($process.ExitCode -ne 0) {
    throw "vim exited with code $($process.ExitCode)"
  }
}

Push-Location $repoRoot

try {
  Write-Host '==> Regenerating keymap docs'
  Invoke-VimCommand -Arguments @(
    '-Nu', 'vimrc/init.vim',
    '-i', 'NONE',
    '-n',
    '-es',
    '-S', 'vimrc/scripts/generate_keymaps_doc.vim',
    '-c', 'qall!'
  )

  Write-Host '==> Validating keymaps.md encoding'
  $docBytes = [System.IO.File]::ReadAllBytes($docPath)
  if ($docBytes -contains 0) {
    throw 'keymaps.md contains NUL bytes'
  }
  if (-not (Test-ByteSequence -Data $docBytes -Pattern ([byte[]](10)))) {
    throw 'keymaps.md does not contain LF line endings'
  }
  if (Test-ByteSequence -Data $docBytes -Pattern ([byte[]](13, 10))) {
    throw 'keymaps.md contains CRLF line endings'
  }

  Write-Host '==> Running git diff --check'
  $mappingPaths = Get-ChildItem -Path (Join-Path $repoRoot 'vimrc/config/mapping') -Filter *.vim |
    Sort-Object Name |
    ForEach-Object { "vimrc/config/mapping/$($_.Name)" }
  $checkPaths = @(
    'vimrc/core/keymap.vim',
    'vimrc/modules/ui/whichkey.vim',
    'vimrc/scripts/generate_keymaps_doc.vim',
    'vimrc/scripts/verify_keymaps.ps1',
    'vimrc/docs/keymaps.md',
    'vimrc/ARCHITECTURE.md',
    'vimrc/README.md'
  )
  $checkPaths = $checkPaths[0], $mappingPaths + $checkPaths[1..($checkPaths.Length - 1)]
  & git diff --check -- @checkPaths
  if ($LASTEXITCODE -ne 0) {
    throw "git diff --check failed with code $LASTEXITCODE"
  }

  Write-Host '==> Keymap verification passed'
}
finally {
  Pop-Location
}
