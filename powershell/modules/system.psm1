# ============================================================================
# 系统管理模块
# 作者: mcge <mcgeq@outlook.com>
# ============================================================================

function Update-AllPackages {
    <#
    .SYNOPSIS
        更新所有包管理器的软件包
    #>
    Write-Host "`n=== System Package Update ===" -ForegroundColor Magenta
    
    # 更新 pip
    if (Get-Command pip -ErrorAction SilentlyContinue) {
        Write-Host "`nStep 1: Updating pip packages..." -ForegroundColor Cyan
        try {
            $outdated = pip list --outdated 2>&1
            if ($outdated -and $outdated.Count -gt 2) {
                $packages = $outdated[2..($outdated.Count - 1)] | ForEach-Object {
                    ($_ -split '\s+')[0]
                }
                
                foreach ($package in $packages) {
                    Write-Host "  Upgrading: $package" -ForegroundColor Yellow
                    pip install -U $package
                }
                Write-Host "✓ Pip packages updated!" -ForegroundColor Green
            } else {
                Write-Host "  All pip packages are up to date" -ForegroundColor Green
            }
        }
        catch {
            Write-Warning "Failed to update pip packages: $_"
        }
    }
    
    # 更新 TeX Live
    if (Get-Command tlmgr -ErrorAction SilentlyContinue) {
        $year = Get-Date -Format yyyy
        Write-Host "`nStep 2: Updating TeX Live $year..." -ForegroundColor Cyan
        try {
            tlmgr update --self
            tlmgr update --all
            Write-Host "✓ TeX Live updated!" -ForegroundColor Green
        }
        catch {
            Write-Warning "Failed to update TeX Live: $_"
        }
    }
    
    # 更新 Chocolatey
    if (Get-Command choco -ErrorAction SilentlyContinue) {
        Write-Host "`nStep 3: Checking Chocolatey updates..." -ForegroundColor Cyan
        try {
            choco outdated
            Write-Host "✓ Chocolatey check completed!" -ForegroundColor Green
        }
        catch {
            Write-Warning "Failed to check Chocolatey updates: $_"
        }
    }
    
    # 更新 Scoop
    if (Get-Command scoop -ErrorAction SilentlyContinue) {
        Write-Host "`nStep 4: Updating Scoop packages..." -ForegroundColor Cyan
        try {
            scoop update *
            Write-Host "✓ Scoop packages updated!" -ForegroundColor Green
        }
        catch {
            Write-Warning "Failed to update Scoop packages: $_"
        }
    }
    
    # 更新 Winget
    if (Get-Command winget -ErrorAction SilentlyContinue) {
        Write-Host "`nStep 5: Updating Winget packages..." -ForegroundColor Cyan
        try {
            winget upgrade --all
            Write-Host "✓ Winget packages updated!" -ForegroundColor Green
        }
        catch {
            Write-Warning "Failed to update Winget packages: $_"
        }
    }
    
    Write-Host "`n=== ✓ Update Complete! ===" -ForegroundColor Magenta
}

function Get-SystemInfo {
    <#
    .SYNOPSIS
        获取系统信息摘要
    #>
    $os = Get-CimInstance Win32_OperatingSystem
    $cpu = Get-CimInstance Win32_Processor
    $memory = Get-CimInstance Win32_PhysicalMemory | Measure-Object -Property Capacity -Sum
    
    [PSCustomObject]@{
        OS = $os.Caption
        Version = $os.Version
        Architecture = $os.OSArchitecture
        CPU = $cpu.Name
        Cores = $cpu.NumberOfCores
        LogicalProcessors = $cpu.NumberOfLogicalProcessors
        TotalMemoryGB = [math]::Round($memory.Sum / 1GB, 2)
        Uptime = (Get-Date) - $os.LastBootUpTime
    } | Format-List
}

# 导出函数
Export-ModuleMember -Function @(
    'Update-AllPackages',
    'Get-SystemInfo'
)
