# ============================================================================
# 网络管理模块
# 作者: mcge <mcgeq@outlook.com>
# ============================================================================

function Set-SystemProxy {
    <#
    .SYNOPSIS
        设置系统代理
    .PARAMETER ProxyUrl
        代理地址，如 http://localhost:7897
    #>
    [CmdletBinding()]
    param(
        [Parameter(Mandatory)]
        [string]$ProxyUrl
    )
    
    try {
        $proxy = New-Object System.Net.WebProxy($ProxyUrl)
        [System.Net.WebRequest]::DefaultWebProxy = $proxy
        
        $webclient = New-Object System.Net.WebClient
        $webclient.Proxy = $proxy
        
        Write-Host "✓ System proxy set to: $ProxyUrl" -ForegroundColor Green
    }
    catch {
        Write-Error "Failed to set proxy: $_"
    }
}

function Remove-SystemProxy {
    <#
    .SYNOPSIS
        移除系统代理设置
    #>
    try {
        [System.Net.WebRequest]::DefaultWebProxy = $null
        Write-Host "✓ System proxy removed" -ForegroundColor Green
    }
    catch {
        Write-Error "Failed to remove proxy: $_"
    }
}

function Get-NetworkAdapters {
    <#
    .SYNOPSIS
        获取所有网络适配器
    #>
    Get-NetAdapter | Sort-Object -Property MacAddress | Format-Table
}

function Get-ActiveIPv4Routes {
    <#
    .SYNOPSIS
        获取活动的 IPv4 路由
    #>
    Get-NetRoute -AddressFamily IPv4 | 
        Where-Object { $_.NextHop -ne '0.0.0.0' } |
        Format-Table
}

function Get-ActiveIPv6Routes {
    <#
    .SYNOPSIS
        获取活动的 IPv6 路由
    #>
    Get-NetRoute -AddressFamily IPv6 | 
        Where-Object { $_.NextHop -ne '::' } |
        Format-Table
}

function Test-InternetConnection {
    <#
    .SYNOPSIS
        测试网络连接
    .PARAMETER Target
        测试目标，默认 8.8.8.8
    #>
    [CmdletBinding()]
    param(
        [string]$Target = "8.8.8.8"
    )
    
    try {
        $result = Test-Connection -ComputerName $Target -Count 4 -ErrorAction Stop
        Write-Host "✓ Connected to $Target" -ForegroundColor Green
        $result | Format-Table
    }
    catch {
        Write-Warning "Failed to connect to $Target"
    }
}

function Get-PublicIP {
    <#
    .SYNOPSIS
        获取公网 IP 地址
    #>
    try {
        $ip = (Invoke-RestMethod -Uri "https://api.ipify.org?format=json").ip
        Write-Host "Public IP: $ip" -ForegroundColor Cyan
        return $ip
    }
    catch {
        Write-Warning "Failed to get public IP: $_"
    }
}

# 导出函数
Export-ModuleMember -Function @(
    'Set-SystemProxy',
    'Remove-SystemProxy',
    'Get-NetworkAdapters',
    'Get-ActiveIPv4Routes',
    'Get-ActiveIPv6Routes',
    'Test-InternetConnection',
    'Get-PublicIP'
)
