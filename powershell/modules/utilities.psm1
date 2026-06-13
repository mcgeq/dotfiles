# ============================================================================
# 工具函数模块
# 作者: mcge <mcgeq@outlook.com>
# ============================================================================

function New-Directory {
    <#
    .SYNOPSIS
        创建目录（类似 mkdir -p）
    .PARAMETER Path
        目录路径
    #>
    [CmdletBinding()]
    param(
        [Parameter(Mandatory)]
        [string]$Path
    )
    
    New-Item -ItemType Directory -Path $Path -Force | Out-Null
    Write-Host "✓ Created directory: $Path" -ForegroundColor Green
}

function New-File {
    <#
    .SYNOPSIS
        创建文件或更新时间戳（类似 touch）
    .PARAMETER Path
        文件路径
    #>
    [CmdletBinding()]
    param(
        [Parameter(Mandatory)]
        [string]$Path
    )
    
    if (-not (Test-Path $Path)) {
        New-Item -ItemType File -Path $Path -Force | Out-Null
        Write-Host "✓ Created file: $Path" -ForegroundColor Green
    } else {
        (Get-Item $Path).LastWriteTime = Get-Date
        Write-Host "✓ Updated timestamp: $Path" -ForegroundColor Yellow
    }
}

function Remove-ItemSafe {
    <#
    .SYNOPSIS
        安全删除文件或目录
    .PARAMETER Path
        要删除的路径
    .PARAMETER Force
        强制删除
    .PARAMETER Recurse
        递归删除
    #>
    [CmdletBinding(SupportsShouldProcess)]
    param(
        [Parameter(Mandatory, Position = 0)]
        [string]$Path,
        
        [switch]$Force,
        [switch]$Recurse
    )
    
    if (-not (Test-Path $Path)) {
        Write-Warning "Path does not exist: $Path"
        return
    }
    
    $params = @{
        Path = $Path
    }
    
    if ($Force) { $params['Force'] = $true }
    if ($Recurse) { $params['Recurse'] = $true }
    
    if ($PSCmdlet.ShouldProcess($Path, "Delete")) {
        Remove-Item @params
        Write-Host "✓ Deleted: $Path" -ForegroundColor Green
    }
}

function Open-ExplorerHere {
    <#
    .SYNOPSIS
        在资源管理器中打开当前目录
    .PARAMETER Path
        要打开的路径，默认当前目录
    #>
    [CmdletBinding()]
    param(
        [string]$Path = '.'
    )
    
    if (Test-Path $Path) {
        Invoke-Item $Path
    } else {
        Write-Error "Path does not exist: $Path"
    }
}

function Get-DirectoryList {
    <#
    .SYNOPSIS
        列出目录内容（简化版）
    #>
    (Get-ChildItem).Name
    Write-Host ""
}

function Invoke-PdmPython {
    <#
    .SYNOPSIS
        使用 PDM 运行 Python 脚本
    #>
    [CmdletBinding()]
    param(
        [Parameter(ValueFromRemainingArguments)]
        [string[]]$Arguments
    )
    
    if (Get-Command pdm -ErrorAction SilentlyContinue) {
        pdm run python @Arguments
    } else {
        Write-Error "PDM not found! Please install it first."
    }
}

function Invoke-UvPython {
    <#
    .SYNOPSIS
        使用 UV 运行 Python 脚本
    #>
    [CmdletBinding()]
    param(
        [Parameter(ValueFromRemainingArguments)]
        [string[]]$Arguments
    )
    
    if (Get-Command uv -ErrorAction SilentlyContinue) {
        uv run python @Arguments
    } else {
        Write-Error "UV not found! Please install it first."
    }
}

# 导出函数
Export-ModuleMember -Function @(
    'New-Directory',
    'New-File',
    'Remove-ItemSafe',
    'Open-ExplorerHere',
    'Get-DirectoryList',
    'Invoke-PdmPython',
    'Invoke-UvPython'
)
