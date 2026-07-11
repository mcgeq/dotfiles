param(
    [switch]$Execute,
    [switch]$AllowDirty,
    [switch]$RemoveLeftovers,
    [switch]$UpdateAfterRestore,
    [switch]$StopOnError,
    [int]$RetryCount = 2,
    [int]$Jobs = 8,
    [string[]]$Only = @()
)

Set-StrictMode -Version Latest
$ErrorActionPreference = "Stop"

function Invoke-Git {
    param(
        [Parameter(Mandatory = $true)]
        [string[]]$Args
    )

    if (-not $Execute) {
        Write-Host "DRY-RUN: git $($Args -join ' ')"
        return @()
    }

    & git @Args
    if ($LASTEXITCODE -ne 0) {
        throw "git $($Args -join ' ') failed with exit code $LASTEXITCODE"
    }
}

function Get-GitOutput {
    param(
        [Parameter(Mandatory = $true)]
        [string[]]$Args
    )

    $output = & git @Args
    if ($LASTEXITCODE -ne 0) {
        throw "git $($Args -join ' ') failed with exit code $LASTEXITCODE"
    }

    return @($output)
}

function Get-OptionalGitOutput {
    param(
        [Parameter(Mandatory = $true)]
        [string[]]$Args
    )

    $output = & git @Args 2>$null
    if ($LASTEXITCODE -ne 0) {
        return @()
    }

    return @($output)
}

function Remove-EmptyParentDirectories {
    param(
        [Parameter(Mandatory = $true)]
        [string]$Path
    )

    $current = Split-Path -Parent $Path
    while ($current -and (Test-Path -LiteralPath $current)) {
        $items = @(Get-ChildItem -LiteralPath $current -Force)
        if ($items.Count -ne 0) {
            break
        }

        Remove-Item -LiteralPath $current -Force
        $current = Split-Path -Parent $current
    }
}

$repoRoot = @(Get-GitOutput @("rev-parse", "--show-toplevel"))[0]
Set-Location -LiteralPath $repoRoot

if (-not (Test-Path -LiteralPath ".gitmodules")) {
    throw "No .gitmodules found in $repoRoot"
}

$configLines = Get-GitOutput @("config", "--file", ".gitmodules", "--get-regexp", "^submodule\..*\.(path|url|branch)$")
$moduleMap = @{}

foreach ($line in $configLines) {
    $parts = $line -split "\s+", 2
    if ($parts.Count -ne 2) {
        continue
    }

    $key = $parts[0]
    $value = $parts[1]
    if ($key -notmatch "^submodule\.(.+)\.(path|url|branch)$") {
        continue
    }

    $name = $Matches[1]
    $field = $Matches[2]
    if (-not $moduleMap.ContainsKey($name)) {
        $moduleMap[$name] = [ordered]@{}
    }

    $moduleMap[$name][$field] = $value
}

$submodules = foreach ($name in $moduleMap.Keys) {
    $entry = $moduleMap[$name]
    if (-not $entry.Contains("path") -or -not $entry.Contains("url")) {
        Write-Host "Skipping incomplete submodule entry: $name" -ForegroundColor Yellow
        continue
    }

    [pscustomobject]@{
        Name = $name
        Path = $entry["path"]
        Url = $entry["url"]
        Branch = if ($entry.Contains("branch")) { $entry["branch"] } else { $null }
    }
}

if ($Only.Count -gt 0) {
    $selected = @($submodules | Where-Object { $Only -contains $_.Name -or $Only -contains $_.Path })
} else {
    $selected = @($submodules)
}

if ($selected.Count -eq 0) {
    throw "No matching submodules found."
}

if ($Execute -and -not $AllowDirty) {
    $status = @(Get-GitOutput @("status", "--porcelain"))
    if ($status.Count -gt 0) {
        $selectedPaths = @($selected | ForEach-Object { $_.Path })
        $nonSubmoduleChanges = @(
            foreach ($line in $status) {
            if ($line.Length -lt 4) {
                $line
                continue
            }

            $pathPart = $line.Substring(3)
            if ($pathPart -match " -> ") {
                $pathPart = ($pathPart -split " -> ", 2)[1]
            }
            if ($pathPart.StartsWith('"') -and $pathPart.EndsWith('"')) {
                $pathPart = $pathPart.Trim('"')
            }

            $isInsideSelected = $false
            foreach ($prefix in $selectedPaths) {
                if ($pathPart -eq $prefix -or $pathPart.StartsWith($prefix + "/")) {
                    $isInsideSelected = $true
                    break
                }
            }

            if (-not $isInsideSelected -and $pathPart -ne ".gitmodules" -and $pathPart -ne "restore-submodules.ps1") {
                $line
            }
        }
        )

        if ($nonSubmoduleChanges.Count -gt 0) {
            Write-Host "Working tree has unrelated changes. Commit/stash/revert them first, or pass -AllowDirty to override." -ForegroundColor Yellow
            $nonSubmoduleChanges | Select-Object -First 40 | ForEach-Object { Write-Host $_ }
            if ($nonSubmoduleChanges.Count -gt 40) {
                Write-Host "... plus $($nonSubmoduleChanges.Count - 40) more entries"
            }
            exit 1
        }
    }
}

$failures = @()
$mode = if ($Execute) { "EXECUTE" } else { "DRY-RUN" }
Write-Host "$mode restore for $($selected.Count) submodule(s) in $repoRoot"

foreach ($submodule in $selected) {
    try {
        Write-Host ""
        Write-Host "==> $($submodule.Name) -> $($submodule.Path)"

        $stageLines = @(Get-GitOutput @("ls-files", "--stage", "--", $submodule.Path))
        $alreadyGitlink = $stageLines | Where-Object { $_ -match "^160000\s" } | Select-Object -First 1
        if ($alreadyGitlink) {
            Write-Host "Already registered as a submodule; skipping git rm/submodule add."
            continue
        }

        if ($stageLines.Count -gt 0) {
            Invoke-Git @("rm", "-r", "-f", "--", $submodule.Path)
        } else {
            Write-Host "No tracked files found at $($submodule.Path)."
        }

        $gitFile = Join-Path $submodule.Path ".git"
        if (Test-Path -LiteralPath $gitFile) {
            if ($Execute) {
                Write-Host "Removing stale nested .git file: $gitFile"
                Remove-Item -LiteralPath $gitFile -Force
            } else {
                Write-Host "DRY-RUN: Remove stale nested .git file: $gitFile"
            }
        }

        if (-not $Execute) {
            Write-Host "DRY-RUN: Remove leftover directory if git rm leaves untracked files: $($submodule.Path)"
        } elseif (Test-Path -LiteralPath $submodule.Path) {
            $leftovers = @(Get-ChildItem -LiteralPath $submodule.Path -Force -Recurse)
            if ($leftovers.Count -gt 0) {
                if (-not $RemoveLeftovers) {
                    throw "Leftover untracked files remain in $($submodule.Path). Re-run with -RemoveLeftovers if you want the script to delete them before adding the submodule."
                }

                Write-Host "Removing leftover directory: $($submodule.Path)"
                Remove-Item -LiteralPath $submodule.Path -Recurse -Force
            } else {
                Remove-Item -LiteralPath $submodule.Path -Force
                Remove-EmptyParentDirectories -Path $submodule.Path
            }
        }

        $addArgs = @("submodule", "add", "--force", "--name", $submodule.Name)
        if ($submodule.Branch) {
            $addArgs += @("-b", $submodule.Branch)
        }
        $addArgs += @($submodule.Url, $submodule.Path)

        $attempt = 1
        while ($true) {
            try {
                Invoke-Git $addArgs
                break
            } catch {
                if (-not $Execute -or $attempt -gt $RetryCount) {
                    throw
                }

                Write-Host "Retry $attempt/$RetryCount for $($submodule.Name) after clone/add failure..." -ForegroundColor Yellow
                Start-Sleep -Seconds ([Math]::Min(10, 2 * $attempt))
                $attempt += 1
            }
        }
    } catch {
        $message = $_.Exception.Message
        $failures += [pscustomobject]@{
            Name = $submodule.Name
            Path = $submodule.Path
            Url = $submodule.Url
            Error = $message
        }

        Write-Host "FAILED: $($submodule.Name) -> $($submodule.Path)" -ForegroundColor Red
        Write-Host $message -ForegroundColor Red
        if ($StopOnError) {
            throw
        }
    }
}

Write-Host ""
Invoke-Git @("submodule", "sync", "--recursive")

if ($UpdateAfterRestore) {
    Invoke-Git @("submodule", "update", "--init", "--recursive", "--remote", "--jobs", "$Jobs")
} else {
    Write-Host "Skipping remote update. Pass -UpdateAfterRestore to run git submodule update --init --recursive --remote --jobs $Jobs."
}

Write-Host ""
if ($Execute) {
    Write-Host "Done. Review with: git status --short"
    if ($failures.Count -gt 0) {
        Write-Host ""
        Write-Host "Failed submodules:" -ForegroundColor Yellow
        $failures | ForEach-Object {
            Write-Host "  - $($_.Name): $($_.Url)"
        }
        exit 1
    }
} else {
    Write-Host "Dry-run only. To execute:"
    Write-Host "  .\restore-submodules.ps1 -Execute -RemoveLeftovers -UpdateAfterRestore -Jobs $Jobs"
}
