#------------------------------------------  Import Modules BEGIN  ------------------------------------------
# 引入posh-git
# Import-Module posh-git

# 引入 oh-my-posh
# Import-Module oh-my-posh

# 引入 ps-read-line
Import-Module -Name PSReadLine

# terminal-icons
Import-Module -Name Terminal-Icons

#设置PowerShell主题

# Set-PoshPrompt -Theme JanDeDobbeleer
# Invoke-Expression (&starship init powershell)
fnm env --use-on-cd | Out-String | Invoke-Expression

# scoop-search
Invoke-Expression (&scoop-search --hook)

# 删除现有的 rm 别名
Remove-Item Alias:rm -ErrorAction SilentlyContinue

# oh-my-posh
oh-my-posh init pwsh --config $env:POSH_THEMES_PATH\mcgeq.omp.toml | Invoke-Expression

# alias
New-Alias -Name gad -Value gitAdd
New-Alias -Name gst -Value gitStatus
New-Alias -Name gph -Value gitPush
New-Alias -Name gpl -Value gitPull
New-Alias -Name gcz -Value gitCz
New-Alias -Name gcn -Value gitNewCheckout
New-Alias -Name gch -Value gitCheckout
New-Alias -Name gbr -Value gitBranch
New-Alias -Name gme -Value gitMerge
New-Alias -Name gab -Value PullAllBranches
New-Alias -Name gsu -Value GitSubmoduleUpdate
New-Alias -Name gpha -Value GitPushAllBranch
New-Alias -Name gsub -Value gitSubmoduleResetHard

New-Alias -Name ppm -Value pnpm

New-Alias -Name pru -Value PdmRunPython
New-Alias -Name uvr -Value UvRunPython
New-Alias -Name rm  -Value rmDirOrFiles

function gitAdd() {
	git add .
}
function gitStatus() {
	git status
}
function gitPush() {
	git push
}
function gitPull() {
	git pull
}

function gitNewCheckout {
	param (
		[string[]]$branchs
	)
	git checkout -b $($branchs -join ' ')
}

function gitCheckout {
	param (
		[string]$branch
	)
	git checkout $branch
}

function gitBranch() {
	git branch
}

function gitMerge {
	param (
		[string]$branch
	)
	git merge --no-ff $branch
}

function gitCz() {
	pnpm run commit
}

function gitCommit {
	git commit
}
# git pull all branch

function gitSubmoduleResetHard() {
    git submodule foreach 'git reset --hard origin/$(git rev-parse --abbrev-ref HEAD)'
}

function PullAllBranches  {
	param(
        [string]$repositoryPath
    )

    # 检查是否提供了路径参数，如果没有，则使用当前目录
    if (-not $repositoryPath) {
        $repositoryPath = Get-Location
    }

    # 进入到指定的 Git 仓库目录
    Set-Location -Path $repositoryPath

	# 拉取所有远程分支
	git fetch --all

	# 获取所有分支的列表
	$remoteBranches = git branch -r

	# 循环遍历每个远程分支
    foreach ($branch in $remoteBranches) {
        # 提取分支名并修剪空格
        $branchName = $branch.Trim() -replace '^origin/', ''
        
        # 输出正在拉取的分支信息
        Write-Host "Pulling branch $branchName..."
        
        # 拉取该分支并自动创建本地分支
        git checkout -B $branchName "origin/$branchName"
    }
	# 输出完成信息
	Write-Host "All branches pulled successfully."
}

function GitSubmoduleUpdate {
	git submodule update --init --recursive
}

function GitPushAllBranch {
	git push --all origin -u
}

function PdmRunPython {
	param (
		[string[]]$branchs
	)
	pdm run python $($branchs -join ' ')
}

function UvRunPython {
	param (
		[string[]]$argvs
	)
	uv run python $($argvs -join ' ')
}

# 定义设置本地代理的函数
function Set-LocalProxy {
    param (
        [string]$ProxyUrl = "http://localhost:7897"
    )

    $proxy = New-Object System.Net.WebProxy($ProxyUrl)
    [System.Net.WebRequest]::DefaultWebProxy = $proxy

    # 可选：为 WebClient 设置代理
    $webclient = New-Object System.Net.WebClient
    $webclient.Proxy = $proxy

    Write-Host "Local proxy set to $ProxyUrl"
}

# 取消设置的代理
function Remove-LocalProxy {
    [System.Net.WebRequest]::DefaultWebProxy = $null
    Write-Host "Local proxy has been removed."
}

# Remove-LocalProxy

# 调用函数
# Set-LocalProxy -ProxyUrl "http://localhost:7897"


#------------------------------------------  Import Modules END  ------------------------------------------

#------------------------------------------  Set Hot-Keys BEGIN  -------------------------------------------
# 设置预测文本来源为历史记录
Set-PSReadLineOption -PredictionSource History

# 设置 Tab 为菜单补全和 Intellisense
Set-PSReadLineKeyHandler -Key "Tab" -Function MenuComplete

# 设置 Ctrl+d 为退出 PowerShell
Set-PSReadlineKeyHandler -Key "Ctrl+d" -Function ViExit

# 设置 Ctrl+z 为撤销
Set-PSReadLineKeyHandler -Key "Ctrl+z" -Function Undo

# 设置向上键为后向搜索历史记录
Set-PSReadLineKeyHandler -Key UpArrow -Function HistorySearchBackward

# 设置向下键为前向搜索历史纪录
Set-PSReadLineKeyHandler -Key DownArrow -Function HistorySearchForward

#------------------------------------------  Set Hot-Keys BEGIN  ------------------------------------------

#------------------------------------------  Functions      BEGIN  ------------------------------------------
# Python 直接执行
$env:PATHEXT += ";.py"

# 更新系统组件
function Update-Packages {
	# update pip
	Write-Host "Step 1: 更新 pip" -ForegroundColor Magenta -BackgroundColor Cyan
	$a = pip list --outdated
	$num_package = $a.Length - 2
	for ($i = 0; $i -lt $num_package; $i++) {
		$tmp = ($a[2 + $i].Split(" "))[0]
		pip install -U $tmp
	}

	# update TeX Live
	$CurrentYear = Get-Date -Format yyyy
	Write-Host "Step 2: 更新 TeX Live" $CurrentYear -ForegroundColor Magenta -BackgroundColor Cyan
	tlmgr update --self
	tlmgr update --all

	# update Chocolotey
	Write-Host "Step 3: 更新 Chocolatey" -ForegroundColor Magenta -BackgroundColor Cyan
	choco outdated
}
#-------------------------------    Functions END     -------------------------------

# custom fun

function rmDirOrFiles {
    param (
        [string]$Path,
        [string[]]$Arguments  # 允许不定数量的参数
    )

    # 确认删除
    if (Test-Path $Path) {
        # 输出确认信息
        # Write-Host "Preparing to delete: '$Path' with arguments: $($Arguments -join ', ')"
        
        # 解析传入的参数
        foreach ($arg in $Arguments) {
            # Write-Host "Processing argument: $arg"
            switch ($arg) {
                '-f' { 
                    # Write-Host "Deleting with -Force..."
                    Remove-Item -Force -Path $Path
                    Write-Host "Deleted '$Path' with -Force"
                    return
                }
                '-r' {
                    # Write-Host "Deleting with -Recurse..."
                    Remove-Item -Recurse -Path $Path
                    Write-Host "Deleted '$Path' with -Recurse"
                    return
                }
                '-rf' { 
                    # Write-Host "Deleting with -Force and -Recurse..."
                    Remove-Item -Force -Recurse -Path $Path
                    Write-Host "Deleted '$Path' with -Force and -Recurse"
                    return
                }
                default { 
                    Write-Host "Unknown argument: $arg"
                    return
                }
            }
        }
    } else {
        Write-Host "Path '$Path' does not exist."
    }
}

# 封装 cd 命令
function cd {
    param (
        [string]$path
    )
    Set-Location -Path $path
}

# 封装 mk 命令
function mk {
    param (
        [string]$path
    )
    New-Item -ItemType Directory -Path $path -Force
}

# 封装 touch 命令
function touch {
    param (
        [string]$path
    )
    # 如果文件不存在，则创建文件；如果存在，则更新其时间戳
    if (-not (Test-Path $path)) {
        New-Item -ItemType File -Path $path -Force
    } else {
        (Get-Item $path).LastWriteTime = Get-Date
    }
}

#-------------------------------   Set Alias BEGIN    -------------------------------
# 1. 编译函数 make
# function MakeThings {
# 	nmake.exe $args -nologo
# }
# Set-Alias -Name make -Value MakeThings

# 2. 更新系统 os-update
Set-Alias -Name os-update -Value Update-Packages

# 3. 查看目录 ls & ll
function ListDirectory {
	(Get-ChildItem).Name
	Write-Host("")
}
Set-Alias -Name ls -Value ListDirectory
Set-Alias -Name ll -Value Get-ChildItem

# 4. 打开当前工作目录
function OpenCurrentFolder {
	param
	(
		# 输入要打开的路径
		# 用法示例：open C:\
		# 默认路径：当前工作文件夹
		$Path = '.'
	)
	Invoke-Item $Path
}
Set-Alias -Name open -Value OpenCurrentFolder
#-------------------------------    Set Alias END     -------------------------------


#-------------------------------   Set Network BEGIN    -------------------------------
# 1. 获取所有 Network Interface
function Get-AllNic {
	Get-NetAdapter | Sort-Object -Property MacAddress
}
Set-Alias -Name getnic -Value Get-AllNic

# 2. 获取 IPv4 关键路由
function Get-IPv4Routes {
	Get-NetRoute -AddressFamily IPv4 | Where-Object -FilterScript { $_.NextHop -ne '0.0.0.0' }
}
Set-Alias -Name getip -Value Get-IPv4Routes

# 3. 获取 IPv6 关键路由
function Get-IPv6Routes {
	Get-NetRoute -AddressFamily IPv6 | Where-Object -FilterScript { $_.NextHop -ne '::' }
}
Set-Alias -Name getip6 -Value Get-IPv6Routes
#-------------------------------    Set Network END     -------------------------------

fnm env --use-on-cd | Out-String | Invoke-Expression
