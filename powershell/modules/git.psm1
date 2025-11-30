# ============================================================================
# Git 功能模块
# 作者: mcge <mcgeq@outlook.com>
# ============================================================================

#region 基础 Git 操作

function Invoke-GitAdd {
    <#
    .SYNOPSIS
        暂存所有更改
    #>
    git add .
}

function Invoke-GitStatus {
    <#
    .SYNOPSIS
        查看 Git 状态
    #>
    git status
}

function Invoke-GitPush {
    <#
    .SYNOPSIS
        推送到远程仓库
    #>
    [CmdletBinding()]
    param(
        [switch]$All,
        [switch]$Force
    )
    
    $params = @()
    if ($All) { $params += "--all" }
    if ($Force) { $params += "--force" }
    
    git push @params
}

function Invoke-GitPull {
    <#
    .SYNOPSIS
        从远程仓库拉取
    #>
    [CmdletBinding()]
    param(
        [switch]$Rebase
    )
    
    if ($Rebase) {
        git pull --rebase
    } else {
        git pull
    }
}

function Invoke-GitCommit {
    <#
    .SYNOPSIS
        提交更改
    #>
    [CmdletBinding()]
    param(
        [Parameter(Mandatory)]
        [string]$Message
    )
    
    git commit -m $Message
}

#endregion

#region 分支管理

function New-GitBranch {
    <#
    .SYNOPSIS
        创建并切换到新分支
    #>
    [CmdletBinding()]
    param(
        [Parameter(Mandatory)]
        [string]$BranchName
    )
    
    git checkout -b $BranchName
}

function Switch-GitBranch {
    <#
    .SYNOPSIS
        切换分支
    #>
    [CmdletBinding()]
    param(
        [Parameter(Mandatory)]
        [string]$BranchName
    )
    
    git checkout $BranchName
}

function Get-GitBranches {
    <#
    .SYNOPSIS
        列出所有分支
    #>
    [CmdletBinding()]
    param(
        [switch]$All,
        [switch]$Remote
    )
    
    $params = @()
    if ($All) { $params += "-a" }
    if ($Remote) { $params += "-r" }
    
    git branch @params
}

function Merge-GitBranch {
    <#
    .SYNOPSIS
        合并分支（no-ff 模式）
    #>
    [CmdletBinding()]
    param(
        [Parameter(Mandatory)]
        [string]$BranchName
    )
    
    git merge --no-ff $BranchName
}

function Sync-AllBranches {
    <#
    .SYNOPSIS
        拉取所有远程分支
    #>
    [CmdletBinding()]
    param(
        [string]$RepositoryPath = $PWD
    )
    
    Push-Location $RepositoryPath
    try {
        Write-Host "Fetching all branches..." -ForegroundColor Cyan
        git fetch --all
        
        $remoteBranches = git branch -r | Where-Object { $_ -notmatch 'HEAD' }
        
        foreach ($branch in $remoteBranches) {
            $branchName = $branch.Trim() -replace '^origin/', ''
            Write-Host "Pulling branch: $branchName" -ForegroundColor Yellow
            git checkout -B $branchName "origin/$branchName"
        }
        
        Write-Host "✓ All branches synced successfully!" -ForegroundColor Green
    }
    catch {
        Write-Error "Failed to sync branches: $_"
    }
    finally {
        Pop-Location
    }
}

#endregion

#region Submodule 管理

function Initialize-GitSubmodules {
    <#
    .SYNOPSIS
        初始化所有子模块
    #>
    Write-Host "Step 1: Initializing all submodules..." -ForegroundColor Cyan
    git submodule update --init --recursive
    Write-Host "✓ Submodules initialized!" -ForegroundColor Green
}

function Update-GitSubmoduleBranches {
    <#
    .SYNOPSIS
        切换所有子模块到主分支
    #>
    [CmdletBinding()]
    param(
        [string[]]$Branches = @("main", "master", "trunk")
    )
    
    Write-Host "Step 2: Switching all submodules to main branch..." -ForegroundColor Cyan
    
    $branchChecks = $Branches | ForEach-Object {
        "if (git show-ref --verify --quiet refs/remotes/origin/$_) { git checkout $_; exit 0; }"
    } | Join-String -Separator " elseif "
    
    git submodule foreach "
        $branchChecks
        else { Write-Host '⚠ No suitable branch found for `$name' -ForegroundColor Yellow; }
    "
    
    Write-Host "✓ Submodules branch switched!" -ForegroundColor Green
}

function Update-GitSubmodules {
    <#
    .SYNOPSIS
        更新所有子模块
    #>
    Write-Host "Step 3: Pulling latest updates for all submodules..." -ForegroundColor Cyan
    git submodule foreach 'git pull --rebase 2>$null || Write-Host "⚠ Failed to pull $name" -ForegroundColor Yellow'
    Write-Host "✓ All submodules updated!" -ForegroundColor Green
}

function Sync-GitSubmodules {
    <#
    .SYNOPSIS
        一键同步所有子模块（初始化 + 切换分支 + 更新）
    #>
    Write-Host "`n=== Syncing All Submodules ===" -ForegroundColor Magenta
    
    Initialize-GitSubmodules
    Write-Host ""
    
    Update-GitSubmoduleBranches
    Write-Host ""
    
    Update-GitSubmodules
    Write-Host ""
    
    Write-Host "=== ✓ All Done! ===" -ForegroundColor Magenta
}

function Reset-GitSubmodules {
    <#
    .SYNOPSIS
        重置所有子模块到远程 HEAD
    #>
    Write-Host "Resetting all submodules..." -ForegroundColor Cyan
    git submodule foreach 'git reset --hard origin/$(git rev-parse --abbrev-ref HEAD)'
    Write-Host "✓ Submodules reset completed!" -ForegroundColor Green
}

#endregion

#region 高级功能

function Push-AllGitBranches {
    <#
    .SYNOPSIS
        推送所有分支到远程
    #>
    Write-Host "Pushing all branches..." -ForegroundColor Cyan
    git push --all origin -u
    Write-Host "✓ All branches pushed!" -ForegroundColor Green
}

function Invoke-ConventionalCommit {
    <#
    .SYNOPSIS
        使用 commitizen 进行规范提交
    #>
    if (Get-Command pnpm -ErrorAction SilentlyContinue) {
        pnpm run commit
    } else {
        Write-Warning "pnpm not found! Please install it first."
    }
}

#endregion

# 导出函数
Export-ModuleMember -Function @(
    # 基础操作
    'Invoke-GitAdd',
    'Invoke-GitStatus', 
    'Invoke-GitPush',
    'Invoke-GitPull',
    'Invoke-GitCommit',
    
    # 分支管理
    'New-GitBranch',
    'Switch-GitBranch',
    'Get-GitBranches',
    'Merge-GitBranch',
    'Sync-AllBranches',
    
    # Submodule 管理
    'Initialize-GitSubmodules',
    'Update-GitSubmoduleBranches',
    'Update-GitSubmodules',
    'Sync-GitSubmodules',
    'Reset-GitSubmodules',
    
    # 高级功能
    'Push-AllGitBranches',
    'Invoke-ConventionalCommit'
)
