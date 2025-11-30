# ============================================================================
# PowerShell Profile - Modular Edition
# 作者: mcge <mcgeq@outlook.com>
# ============================================================================

#region 初始化

# 获取配置文件目录
$ProfileDir = Split-Path -Parent $PROFILE
$ModulesDir = Join-Path $ProfileDir "modules"
$ConfigDir = Join-Path $ProfileDir "config"

# 加载配置
$Config = Import-PowerShellDataFile -Path (Join-Path $ConfigDir "settings.psd1")

#endregion

#region 模块导入

# 导入核心模块
if ($Config.Modules.PSReadLine) {
    Import-Module -Name PSReadLine -ErrorAction SilentlyContinue
}

if ($Config.Modules.TerminalIcons) {
    Import-Module -Name Terminal-Icons -ErrorAction SilentlyContinue
}

# 导入自定义模块
$customModules = @(
    'git',
    'jujutsu',
    'utilities',
    'system',
    'network'
)

foreach ($module in $customModules) {
    $modulePath = Join-Path $ModulesDir "$module.psm1"
    if (Test-Path $modulePath) {
        Import-Module $modulePath -Force
    } else {
        Write-Warning "Module not found: $modulePath"
    }
}

#endregion

#region 外部工具初始化

# Starship 提示符
if ($Config.Modules.Starship -and (Get-Command starship -ErrorAction SilentlyContinue)) {
    Invoke-Expression (&starship init powershell)
}

# fnm (Node.js 版本管理)
if ($Config.Modules.Fnm -and (Get-Command fnm -ErrorAction SilentlyContinue)) {
    fnm env --use-on-cd | Out-String | Invoke-Expression
}

# Scoop Search
if ($Config.Modules.ScoopSearch -and (Get-Command scoop-search -ErrorAction SilentlyContinue)) {
    Invoke-Expression (&scoop-search --hook)
}

#endregion

#region PSReadLine 配置

if ($Config.Modules.PSReadLine) {
    # 设置预测文本来源
    Set-PSReadLineOption -PredictionSource $Config.PSReadLine.PredictionSource
    
    # 设置编辑模式
    Set-PSReadLineOption -EditMode $Config.PSReadLine.EditMode
    
    # 快捷键绑定
    Set-PSReadLineKeyHandler -Key "Tab" -Function MenuComplete
    Set-PSReadLineKeyHandler -Key "Ctrl+d" -Function ViExit
    Set-PSReadLineKeyHandler -Key "Ctrl+z" -Function Undo
    Set-PSReadLineKeyHandler -Key UpArrow -Function HistorySearchBackward
    Set-PSReadLineKeyHandler -Key DownArrow -Function HistorySearchForward
}

#endregion

#region 环境变量

# Python 可执行扩展名
$env:PATHEXT += ";.PY"

#endregion

#region 别名定义

# 移除冲突的别名（保留 cd，它是 PowerShell 内置别名）
Remove-Item Alias:rm -ErrorAction SilentlyContinue

# Git 别名
Set-Alias -Name gad -Value Invoke-GitAdd
Set-Alias -Name gst -Value Invoke-GitStatus
Set-Alias -Name gph -Value Invoke-GitPush
Set-Alias -Name gpl -Value Invoke-GitPull
Set-Alias -Name gco -Value Invoke-GitCommit
Set-Alias -Name gcn -Value New-GitBranch
Set-Alias -Name gch -Value Switch-GitBranch
Set-Alias -Name gbr -Value Get-GitBranches
Set-Alias -Name gme -Value Merge-GitBranch
Set-Alias -Name gcz -Value Invoke-ConventionalCommit

# Git 分支与 Submodule
Set-Alias -Name gab -Value Sync-AllBranches
Set-Alias -Name gpha -Value Push-AllGitBranches
Set-Alias -Name gsub -Value Reset-GitSubmodules
Set-Alias -Name gsi -Value Initialize-GitSubmodules
Set-Alias -Name gsub -Value Update-GitSubmoduleBranches
Set-Alias -Name gspl -Value Update-GitSubmodules
Set-Alias -Name gsync -Value Sync-GitSubmodules

# Jujutsu 别名
Set-Alias -Name jad -Value Invoke-JujutsuAdd
Set-Alias -Name jst -Value Get-JujutsuStatus
Set-Alias -Name jco -Value Invoke-JujutsuCommit
Set-Alias -Name jlg -Value Get-JujutsuLog
Set-Alias -Name jdf -Value Get-JujutsuDiff
Set-Alias -Name jch -Value Switch-JujutsuRevision
Set-Alias -Name jme -Value Merge-JujutsuRevision
Set-Alias -Name jgf -Value Invoke-JujutsuFetch
Set-Alias -Name jfm -Value Sync-JujutsuMain
Set-Alias -Name jbm -Value Set-JujutsuBookmark
Set-Alias -Name jrm -Value Invoke-JujutsuRebase
Set-Alias -Name jjp -Value Invoke-JujutsuGitPush

# 工具函数别名
Set-Alias -Name mk -Value New-Directory
Set-Alias -Name touch -Value New-File
Set-Alias -Name rm -Value Remove-ItemSafe
Set-Alias -Name open -Value Open-ExplorerHere
Set-Alias -Name ls -Value Get-DirectoryList
Set-Alias -Name ll -Value Get-ChildItem

# Python 工具别名
Set-Alias -Name pru -Value Invoke-PdmPython
Set-Alias -Name uvr -Value Invoke-UvPython

# 其他别名
Set-Alias -Name ppm -Value pnpm
Set-Alias -Name os-update -Value Update-AllPackages
Set-Alias -Name sysinfo -Value Get-SystemInfo

# 网络别名
Set-Alias -Name getnic -Value Get-NetworkAdapters
Set-Alias -Name getip -Value Get-ActiveIPv4Routes
Set-Alias -Name getip6 -Value Get-ActiveIPv6Routes
Set-Alias -Name myip -Value Get-PublicIP

#endregion

#region 启动信息

Write-Host "`n✨ PowerShell Profile Loaded (Modular Edition)" -ForegroundColor Green
Write-Host "   Type 'Get-Command -Module git,jujutsu,utilities,system,network' to see available commands`n" -ForegroundColor Yellow

#endregion
