<#
.SYNOPSIS
    Migrate Emacs extensions to new directory structure

.DESCRIPTION
    Reorganize site-lisp/extensions/ directory by categorizing 
    all Git submodules into functional groups.

.NOTES
    Author: mcge
    Date: 2025-12-01
    Version: 1.0
#>

param(
    [switch]$DryRun,
    [switch]$Backup,
    [string]$BackupPath = ".\backup"
)

$ErrorActionPreference = "Stop"

# Color output functions
function Write-ColorOutput {
    param([string]$Message, [string]$Color = "White")
    Write-Host $Message -ForegroundColor $Color
}

function Write-Success { param([string]$M) Write-Host "OK: $M" -ForegroundColor Green }
function Write-Fail { param([string]$M) Write-Host "ERROR: $M" -ForegroundColor Red }
function Write-Warn { param([string]$M) Write-Host "WARN: $M" -ForegroundColor Yellow }
function Write-Info { param([string]$M) Write-Host "INFO: $M" -ForegroundColor Cyan }

# Check Git status
function Test-GitClean {
    $status = git status --porcelain
    if ($status) {
        Write-Warn "Git working directory is not clean"
        Write-Info "Run 'git status' for details"
        return $false
    }
    return $true
}

# Create backup
function Backup-Current {
    param([string]$Path)
    
    if ($Backup) {
        Write-Info "Creating backup..."
        
        if (Test-Path $Path) {
            Remove-Item $Path -Recurse -Force
        }
        New-Item -ItemType Directory -Path $Path -Force | Out-Null
        
        Copy-Item .\.gitmodules "$Path\.gitmodules" -Force
        Copy-Item .\emacs\site-lisp\extensions "$Path\extensions" -Recurse -Force
        
        Write-Success "Backup completed: $Path"
    }
}

# Extension category mapping
$ExtensionCategories = @{
    # Core libraries
    "dash" = "core"
    "s.el" = "core"
    "f.el" = "core"
    "ht" = "core"
    "popup-el" = "core"
    "lazy-load" = "core"
    
    # Completion system
    "vertico" = "completion"
    "marginalia" = "completion"
    "orderless" = "completion"
    "embark" = "completion"
    "consult" = "completion"
    "pinyinlib" = "completion"
    
    # Editor enhancements
    "beacon" = "editor"
    "symbol-overlay" = "editor"
    "vundo" = "editor"
    "fingertip" = "editor"
    "wraplish" = "editor"
    "markmacro" = "editor"
    "ws-butler" = "editor"
    
    # Version control
    "magit" = "git"
    "with-editor" = "git"
    "jujutsu" = "git"
    "llama" = "git"
    
    # LSP and language support (keep subdirectory structure)
    "lsp" = "lsp"
    
    # Org Mode (keep subdirectory structure)
    "org" = "org"
    
    # UI and themes
    "doom-modeline" = "ui"
    "shrink-path" = "ui"
    "solaire-mode" = "ui"
    "themes" = "ui"
    
    # Search tools
    "blink-search" = "search"
    "color-rg" = "search"
    "consult-todo" = "search"
    "hl-todo" = "search"
    
    # Code snippets (keep subdirectory structure)
    "snippets" = "snippets"
    
    # Input method
    "emacs-rime" = "input"
    "posframe" = "input"
    
    # Documentation tools
    "markdown-ts-mode" = "docs"
    "md" = "docs"
    "flymake-vale" = "docs"
    "emacs-htmlize" = "docs"
    
    # Utilities
    "auto-save" = "utils"
    "sort-tab" = "utils"
    "helpful" = "utils"
    "elisp-refs" = "utils"
    "hydra" = "utils"
    "instant-rename-tag" = "utils"
    "highlight-matching-tag" = "utils"
    "json-mode" = "utils"
    "json-snatcher" = "utils"
    "js2-mode" = "utils"
}

# Get current extensions
function Get-CurrentExtensions {
    $extensionsPath = ".\emacs\site-lisp\extensions"
    
    if (-not (Test-Path $extensionsPath)) {
        Write-Fail "Extensions directory not found"
        exit 1
    }
    
    Get-ChildItem $extensionsPath -Directory | ForEach-Object { $_.Name }
}

# Generate migration map
function Get-MigrationMap {
    $currentExtensions = Get-CurrentExtensions
    $migrationMap = @{}
    
    foreach ($ext in $currentExtensions) {
        $category = $ExtensionCategories[$ext]
        
        if (-not $category) {
            Write-Warn "Extension '$ext' not categorized, skipping"
            continue
        }
        
        $oldPath = "emacs/site-lisp/extensions/$ext"
        $newPath = "emacs/site-lisp/extensions/$category/$ext"
        
        $migrationMap[$oldPath] = $newPath
    }
    
    return $migrationMap
}

# Handle nested extensions
function Get-NestedExtensions {
    $nestedMap = @{}
    
    # LSP subdirectories
    $lspPath = ".\emacs\site-lisp\extensions\lsp"
    if (Test-Path $lspPath) {
        Get-ChildItem $lspPath -Directory | ForEach-Object {
            $oldPath = "emacs/site-lisp/extensions/lsp/$($_.Name)"
            $newPath = "emacs/site-lisp/extensions/lsp/$($_.Name)"
            $nestedMap[$oldPath] = $newPath
        }
    }
    
    # Org subdirectories
    $orgPath = ".\emacs\site-lisp\extensions\org"
    if (Test-Path $orgPath) {
        Get-ChildItem $orgPath -Directory | ForEach-Object {
            $oldPath = "emacs/site-lisp/extensions/org/$($_.Name)"
            $newPath = "emacs/site-lisp/extensions/org/$($_.Name)"
            $nestedMap[$oldPath] = $newPath
        }
    }
    
    # Themes subdirectories
    $themesPath = ".\emacs\site-lisp\extensions\themes"
    if (Test-Path $themesPath) {
        Get-ChildItem $themesPath -Directory | ForEach-Object {
            $oldPath = "emacs/site-lisp/extensions/themes/$($_.Name)"
            $newPath = "emacs/site-lisp/extensions/ui/themes/$($_.Name)"
            $nestedMap[$oldPath] = $newPath
        }
    }
    
    # MD subdirectories
    $mdPath = ".\emacs\site-lisp\extensions\md"
    if (Test-Path $mdPath) {
        Get-ChildItem $mdPath -Directory | ForEach-Object {
            $oldPath = "emacs/site-lisp/extensions/md/$($_.Name)"
            $newPath = "emacs/site-lisp/extensions/docs/$($_.Name)"
            $nestedMap[$oldPath] = $newPath
        }
    }
    
    # Snippets subdirectories
    $snippetsPath = ".\emacs\site-lisp\extensions\snippets"
    if (Test-Path $snippetsPath) {
        Get-ChildItem $snippetsPath -Directory | ForEach-Object {
            $oldPath = "emacs/site-lisp/extensions/snippets/$($_.Name)"
            $newPath = "emacs/site-lisp/extensions/snippets/$($_.Name)"
            $nestedMap[$oldPath] = $newPath
        }
    }
    
    return $nestedMap
}

# Execute migration
function Invoke-Migration {
    param(
        [hashtable]$MigrationMap,
        [bool]$IsDryRun
    )
    
    $totalCount = $MigrationMap.Count
    $currentCount = 0
    
    Write-Info "Starting migration of $totalCount extensions..."
    
    foreach ($entry in $MigrationMap.GetEnumerator()) {
        $currentCount++
        $oldPath = $entry.Key
        $newPath = $entry.Value
        
        Write-Progress -Activity "Migrating extensions" -Status "$currentCount / $totalCount" `
            -PercentComplete (($currentCount / $totalCount) * 100)
        
        if ($oldPath -eq $newPath) {
            Write-Info "  [$currentCount/$totalCount] Skip (no move needed): $oldPath"
            continue
        }
        
        if ($IsDryRun) {
            Write-Warn "  [$currentCount/$totalCount] [DRY-RUN] $oldPath -> $newPath"
        } else {
            try {
                # Create target directory
                $newDir = Split-Path $newPath -Parent
                if (-not (Test-Path $newDir)) {
                    New-Item -ItemType Directory -Path $newDir -Force | Out-Null
                }
                
                # Use git mv to move submodule
                Write-Success "  [$currentCount/$totalCount] Moving: $oldPath -> $newPath"
                git mv $oldPath $newPath 2>&1 | Out-Null
                
                if ($LASTEXITCODE -ne 0) {
                    Write-Warn "    git mv failed, trying manual move"
                    Move-Item $oldPath $newPath -Force
                }
            } catch {
                Write-Fail "    Move failed: $_"
            }
        }
    }
    
    Write-Progress -Activity "Migrating extensions" -Completed
}

# Update .gitmodules
function Update-GitModules {
    param([hashtable]$MigrationMap)
    
    Write-Info "Updating .gitmodules..."
    
    $gitmodulesPath = ".\.gitmodules"
    if (-not (Test-Path $gitmodulesPath)) {
        Write-Fail ".gitmodules file not found"
        return
    }
    
    $content = Get-Content $gitmodulesPath -Raw
    
    foreach ($entry in $MigrationMap.GetEnumerator()) {
        $oldPath = $entry.Key
        $newPath = $entry.Value
        
        if ($oldPath -ne $newPath) {
            $content = $content -replace [regex]::Escape("path = $oldPath"), "path = $newPath"
        }
    }
    
    if (-not $DryRun) {
        Set-Content $gitmodulesPath -Value $content -NoNewline
        Write-Success ".gitmodules updated"
    } else {
        Write-Warn "[DRY-RUN] .gitmodules will be updated"
    }
}

# Sync Git Submodules
function Sync-GitSubmodules {
    Write-Info "Syncing Git Submodules..."
    
    if (-not $DryRun) {
        git submodule sync 2>&1 | Out-Null
        if ($LASTEXITCODE -eq 0) {
            Write-Success "Submodules synced"
        } else {
            Write-Warn "Submodules sync failed"
        }
    } else {
        Write-Warn "[DRY-RUN] Would execute: git submodule sync"
    }
}

# Generate migration report
function New-MigrationReport {
    param([hashtable]$MigrationMap)
    
    $reportPath = ".\emacs\MIGRATION_REPORT.txt"
    $report = @"
Emacs Extensions Migration Report
Generated: $(Get-Date -Format 'yyyy-MM-dd HH:mm:ss')
==========================================

Total extensions migrated: $($MigrationMap.Count)

Detailed migration list:
"@
    
    foreach ($entry in $MigrationMap.GetEnumerator() | Sort-Object Value) {
        $report += "`n$($entry.Key)`n  -> $($entry.Value)"
    }
    
    $report += "`n`nBy category:"
    $categories = $MigrationMap.Values | ForEach-Object { ($_ -split '/')[3] } | Group-Object
    foreach ($cat in $categories | Sort-Object Name) {
        $report += "`n  $($cat.Name): $($cat.Count) extensions"
    }
    
    Set-Content $reportPath -Value $report
    Write-Success "Migration report generated: $reportPath"
}

# Main function
function Main {
    Write-ColorOutput "==================================" "Cyan"
    Write-ColorOutput "Emacs Extensions Migration Script" "Cyan"
    Write-ColorOutput "==================================" "Cyan"
    
    # Check if in correct directory
    if (-not (Test-Path ".\emacs\site-lisp\extensions")) {
        Write-Fail "Please run this script from dotfiles root directory"
        exit 1
    }
    
    # Check Git status
    if (-not $DryRun -and -not (Test-GitClean)) {
        $continue = Read-Host "Continue anyway? (y/N)"
        if ($continue -ne 'y') {
            Write-Warn "Migration cancelled"
            exit 0
        }
    }
    
    # Create backup
    if ($Backup) {
        Backup-Current $BackupPath
    }
    
    # Generate migration map
    Write-Info "Analyzing extensions directory..."
    $migrationMap = Get-MigrationMap
    $nestedMap = Get-NestedExtensions
    
    # Merge maps
    foreach ($entry in $nestedMap.GetEnumerator()) {
        $migrationMap[$entry.Key] = $entry.Value
    }
    
    Write-Success "Found $($migrationMap.Count) extensions"
    
    if ($DryRun) {
        Write-Warn ""
        Write-Warn "=== DRY RUN MODE ==="
        Write-Warn "No actual file operations will be performed"
        Write-Warn ""
    }
    
    # Execute migration
    Invoke-Migration -MigrationMap $migrationMap -IsDryRun $DryRun
    
    # Update .gitmodules
    Update-GitModules -MigrationMap $migrationMap
    
    # Sync submodules
    Sync-GitSubmodules
    
    # Generate report
    New-MigrationReport -MigrationMap $migrationMap
    
    # Complete
    Write-ColorOutput "`n==================================" "Green"
    if ($DryRun) {
        Write-Warn "DRY RUN completed!"
        Write-Info "Run without -DryRun to execute actual migration"
    } else {
        Write-Success "Migration completed!"
        Write-Info ""
        Write-Info "Next steps:"
        Write-Info "1. Run: git submodule update --init --recursive"
        Write-Info "2. Update config file (init-loadpath.org)"
        Write-Info "3. Regenerate config: make clean && make generate"
        Write-Info "4. Test Emacs config"
    }
    Write-ColorOutput "==================================" "Green"
}

# Run main function
Main
