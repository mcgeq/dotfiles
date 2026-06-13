@{
    # ============================================================================
    # PowerShell 配置参数
    # 作者: mcge <mcgeq@outlook.com>
    # ============================================================================
    
    # 代理设置
    Proxy = @{
        Url = "http://localhost:7897"
        Enabled = $false
    }
    
    # Git 配置
    Git = @{
        DefaultBranch = "main"
        SubmoduleBranches = @("main", "master", "trunk")
    }
    
    # PSReadLine 配置
    PSReadLine = @{
        PredictionSource = "History"
        EditMode = "Windows"
    }
    
    # 模块配置
    Modules = @{
        PSReadLine = $true
        TerminalIcons = $true
        Starship = $true
        ScoopSearch = $true
        Fnm = $true
    }
}
