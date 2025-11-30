# ============================================================================
# Jujutsu (jj) 功能模块
# 作者: mcge <mcgeq@outlook.com>
# ============================================================================

function Initialize-JujutsuRepo {
    <#
    .SYNOPSIS
        初始化 Jujutsu 仓库
    #>
    jj init
}

function Get-JujutsuStatus {
    <#
    .SYNOPSIS
        查看 Jujutsu 状态
    #>
    jj status
}

function Invoke-JujutsuGitPush {
    <#
    .SYNOPSIS
        推送到 Git 远程仓库
    #>
    jj git push
}

function Invoke-JujutsuFetch {
    <#
    .SYNOPSIS
        从远程获取更新
    #>
    jj git fetch
}

function Invoke-JujutsuAdd {
    <#
    .SYNOPSIS
        添加文件到工作区
    #>
    jj add
}

function Invoke-JujutsuCommit {
    <#
    .SYNOPSIS
        提交更改
    #>
    [CmdletBinding()]
    param(
        [Parameter(Mandatory)]
        [string]$Message
    )
    
    jj commit -m $Message
}

function Get-JujutsuLog {
    <#
    .SYNOPSIS
        查看提交日志
    #>
    [CmdletBinding()]
    param(
        [int]$Limit
    )
    
    if ($Limit) {
        jj log -n $Limit
    } else {
        jj log
    }
}

function Get-JujutsuDiff {
    <#
    .SYNOPSIS
        查看更改差异
    #>
    jj diff
}

function Switch-JujutsuRevision {
    <#
    .SYNOPSIS
        切换到指定修订版本
    #>
    [CmdletBinding()]
    param(
        [Parameter(Mandatory)]
        [string]$Revision
    )
    
    jj checkout $Revision
}

function Merge-JujutsuRevision {
    <#
    .SYNOPSIS
        合并指定修订版本
    #>
    [CmdletBinding()]
    param(
        [Parameter(Mandatory)]
        [string]$Revision
    )
    
    jj merge $Revision
}

function Set-JujutsuBookmark {
    <#
    .SYNOPSIS
        设置书签到 main 分支
    #>
    [CmdletBinding()]
    param(
        [string]$BookmarkName = "main",
        [string]$Revision = '@-'
    )
    
    jj bookmark set $BookmarkName -r $Revision
}

function Invoke-JujutsuRebase {
    <#
    .SYNOPSIS
        将当前更改 rebase 到 main
    #>
    [CmdletBinding()]
    param(
        [string]$Target = "main@origin"
    )
    
    jj rebase -d $Target
}

function Sync-JujutsuMain {
    <#
    .SYNOPSIS
        同步并 rebase 到 main 分支
    #>
    Write-Host "Fetching from remote..." -ForegroundColor Cyan
    Invoke-JujutsuFetch
    
    Write-Host "Rebasing to main..." -ForegroundColor Cyan
    Invoke-JujutsuRebase
    
    Write-Host "✓ Synced with main!" -ForegroundColor Green
}

# 导出函数
Export-ModuleMember -Function @(
    'Initialize-JujutsuRepo',
    'Get-JujutsuStatus',
    'Invoke-JujutsuGitPush',
    'Invoke-JujutsuFetch',
    'Invoke-JujutsuAdd',
    'Invoke-JujutsuCommit',
    'Get-JujutsuLog',
    'Get-JujutsuDiff',
    'Switch-JujutsuRevision',
    'Merge-JujutsuRevision',
    'Set-JujutsuBookmark',
    'Invoke-JujutsuRebase',
    'Sync-JujutsuMain'
)
