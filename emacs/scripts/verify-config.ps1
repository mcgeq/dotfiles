<#
.SYNOPSIS
    验证 Emacs 配置完整性

.DESCRIPTION
    检查 Emacs 配置的完整性，包括：
    - Git submodules 状态
    - 目录结构
    - 配置文件
    - 扩展注册

.NOTES
    Author: mcge
    Date: 2025-12-01
    Version: 1.0
#>

param(
    [switch]$Detailed,
    [switch]$FixIssues
)

$ErrorActionPreference = "Stop"

# 颜色输出
function Write-ColorOutput {
    param(
        [string]$Message,
        [string]$Color = "White",
        [string]$Icon = ""
    )
    if ($Icon) {
        Write-Host "$Icon " -NoNewline -ForegroundColor $Color
    }
    Write-Host $Message -ForegroundColor $Color
}

function Write-Success { param([string]$Message) Write-ColorOutput $Message "Green" "✓" }
function Write-Error { param([string]$Message) Write-ColorOutput $Message "Red" "✗" }
function Write-Warning { param([string]$Message) Write-ColorOutput $Message "Yellow" "⚠" }
function Write-Info { param([string]$Message) Write-ColorOutput $Message "Cyan" "ℹ" }

# 检查计数器
$script:totalChecks = 0
$script:passedChecks = 0
$script:failedChecks = 0
$script:warnings = 0

function Test-Check {
    param(
        [string]$Name,
        [scriptblock]$Test,
        [string]$ErrorMessage,
        [scriptblock]$Fix
    )
    
    $script:totalChecks++
    
    try {
        $result = & $Test
        if ($result) {
            $script:passedChecks++
            Write-Success $Name
            return $true
        } else {
            $script:failedChecks++
            Write-Error "$Name - $ErrorMessage"
            
            if ($FixIssues -and $Fix) {
                Write-Info "  尝试修复..."
                try {
                    & $Fix
                    Write-Success "  已修复"
                } catch {
                    Write-Error "  修复失败: $_"
                }
            }
            return $false
        }
    } catch {
        $script:failedChecks++
        Write-Error "$Name - 检查失败: $_"
        return $false
    }
}

function Test-Warning {
    param(
        [string]$Name,
        [scriptblock]$Test,
        [string]$Message
    )
    
    try {
        $result = & $Test
        if (-not $result) {
            $script:warnings++
            Write-Warning "$Name - $Message"
            return $false
        }
        return $true
    } catch {
        $script:warnings++
        Write-Warning "$Name - 检查异常: $_"
        return $false
    }
}

# ======================================
# 检查项目
# ======================================

Write-ColorOutput "`n================================" "Cyan"
Write-ColorOutput "Emacs 配置验证工具" "Cyan"
Write-ColorOutput "================================`n" "Cyan"

# 1. 检查目录结构
Write-Info "检查目录结构..."

Test-Check -Name "Emacs 目录存在" -Test {
    Test-Path '.\emacs'
} -ErrorMessage "找不到 emacs 目录"

Test-Check -Name "Extensions 目录存在" -Test {
    Test-Path '.\emacs\site-lisp\extensions'
} -ErrorMessage "找不到 extensions 目录"

Test-Check -Name "Config-org 目录存在" -Test {
    Test-Path '.\emacs\config-org'
} -ErrorMessage "找不到 config-org 目录"

Test-Check -Name "Makefile 存在" -Test {
    Test-Path '.\emacs\Makefile'
} -ErrorMessage "找不到 Makefile"

# 2. 检查 Git 配置
Write-Info "`n检查 Git 配置..."

Test-Check -Name ".gitmodules 文件存在" -Test {
    Test-Path '.\.gitmodules'
} -ErrorMessage "找不到 .gitmodules 文件"

Test-Check -Name "Git 工作目录干净" -Test {
    $status = git status --porcelain
    [string]::IsNullOrWhiteSpace($status)
} -ErrorMessage "存在未提交的更改"

# 3. 检查 Git Submodules
Write-Info "`n检查 Git Submodules..."

$submoduleStatus = git submodule status 2>&1
$submoduleLines = $submoduleStatus -split "`n" | Where-Object { $_.Trim() }

$uninitializedCount = ($submoduleLines | Where-Object { $_ -match "^-" }).Count
$modifiedCount = ($submoduleLines | Where-Object { $_ -match "^\+" }).Count
$normalCount = ($submoduleLines | Where-Object { $_ -match "^ " }).Count

Write-Info "  总计: $($submoduleLines.Count) 个 submodules"
Write-Info "  正常: $normalCount"

if ($uninitializedCount -gt 0) {
    Write-Warning "  未初始化: $uninitializedCount"
}

if ($modifiedCount -gt 0) {
    Write-Warning "  已修改: $modifiedCount"
}

Test-Check -Name "所有 Submodules 已初始化" -Test {
    $uninitializedCount -eq 0
} -ErrorMessage "有 $uninitializedCount 个 submodule 未初始化" -Fix {
    git submodule update --init --recursive
}

Test-Warning -Name "Submodules 未被修改" -Test {
    $modifiedCount -eq 0
} -Message "有 $modifiedCount 个 submodule 被修改"

# 4. 检查扩展目录结构（重构后）
Write-Info "`n检查扩展目录结构..."

$expectedCategories = @(
    "core",
    "completion",
    "editor",
    "git",
    "lsp",
    "org",
    "ui",
    "search",
    "snippets",
    "input",
    "docs",
    "utils"
)

$extensionsPath = '.\emacs\site-lisp\extensions'

foreach ($category in $expectedCategories) {
    $categoryPath = Join-Path $extensionsPath $category
    Test-Warning -Name "类别目录存在: $category" -Test {
        Test-Path $categoryPath
    } -Message "$category 目录不存在（可能未完成重构）"
}

# 5. 检查配置文件
Write-Info "`n检查配置文件..."

$configFiles = @(
    "config-org\init.org",
    "config-org\core\init-paths.org",
    "config-org\core\init-loadpath.org",
    "config-org\core\init-const.org",
    "config-org\core\init-font.org",
    "config-org\core\init-function.org",
    "config-org\core\init-builtin.org"
)

foreach ($file in $configFiles) {
    $fullPath = Join-Path ".\emacs" $file
    Test-Check -Name "配置文件: $file" -Test {
        Test-Path $fullPath
    } -ErrorMessage "文件不存在"
}

# 6. 检查生成的配置
Write-Info "`n检查生成的配置..."

$generatedPath = '.\emacs\site-lisp\config'

Test-Check -Name "Config 目录存在" -Test {
    Test-Path $generatedPath
} -ErrorMessage "配置未生成，运行 make generate"

if (Test-Path $generatedPath) {
    Test-Check -Name "init.el 已生成" -Test {
        Test-Path (Join-Path $generatedPath "init.el")
    } -ErrorMessage "init.el 未生成" -Fix {
        Push-Location .\emacs
        make generate
        Pop-Location
    }
    
    # 检查是否有最近的生成
    $initEl = Join-Path $generatedPath "init.el"
    if (Test-Path $initEl) {
        $lastWrite = (Get-Item $initEl).LastWriteTime
        $ageInDays = (Get-Date) - $lastWrite
        $dateStr = $lastWrite.ToString("yyyy-MM-dd HH:mm")
        
        Test-Warning -Name "配置生成时间" -Test {
            $ageInDays.TotalDays -lt 7
        } -Message "配置生成于 $dateStr，可能需要重新生成"
    }
}

# 7. 检查文档文件
Write-Info "`n检查文档文件..."

Test-Check -Name "README.org 存在" -Test {
    Test-Path '.\emacs\README.org'
} -ErrorMessage "README.org 不存在"

if ($Detailed) {
    $docs = @(
        "REFACTOR_PLAN.org",
        "REFACTOR_GUIDE.org",
        "REFACTOR_SUMMARY.md",
        "BEFORE_AFTER.org"
    )
    
    foreach ($doc in $docs) {
        Test-Warning -Name "重构文档: $doc" -Test {
            Test-Path ".\emacs\$doc"
        } -Message "文档不存在（如果已完成重构，此文档应存在）"
    }
}

# 8. 检查脚本文件
Write-Info "`n检查脚本文件..."

$scripts = @(
    "scripts\migrate-extensions.ps1",
    "scripts\verify-config.ps1"
)

foreach ($script in $scripts) {
    $scriptPath = Join-Path ".\emacs" $script
    Test-Warning -Name "脚本: $script" -Test {
        Test-Path $scriptPath
    } -Message "脚本不存在"
}

# 9. 检查特定扩展（示例）
if ($Detailed) {
    Write-Info "`n检查关键扩展..."
    
    $keyExtensions = @{
        "dash" = @("extensions\dash", "extensions\core\dash")
        "vertico" = @("extensions\vertico", "extensions\completion\vertico")
        "magit" = @("extensions\magit", "extensions\git\magit")
        "lsp-bridge" = @("extensions\lsp\lsp-bridge")
    }
    
    foreach ($ext in $keyExtensions.Keys) {
        $found = $false
        foreach ($path in $keyExtensions[$ext]) {
            $fullPath = Join-Path ".\emacs\site-lisp" $path
            if (Test-Path $fullPath) {
                $found = $true
                break
            }
        }
        
        Test-Warning -Name "扩展: $ext" -Test {
            $found
        } -Message "未找到 $ext（可能在不同位置或未安装）"
    }
}

# 10. 性能检查
if ($Detailed) {
    Write-Info "`n性能检查..."
    
    # 检查 extensions 目录大小
    if (Test-Path $extensionsPath) {
        $size = (Get-ChildItem $extensionsPath -Recurse | Measure-Object -Property Length -Sum).Sum / 1MB
        Write-Info "  Extensions 目录大小: $([math]::Round($size, 2)) MB"
        
        Test-Warning -Name "目录大小合理" -Test {
            $size -lt 1000
        } -Message "目录过大 ($([math]::Round($size, 2)) MB)，可能需要清理"
    }
    
    # 检查 submodule 数量
    Write-Info "  Submodule 数量: $($submoduleLines.Count)"
}

# ======================================
# 总结报告
# ======================================

Write-ColorOutput "`n================================" "Cyan"
Write-ColorOutput "验证结果" "Cyan"
Write-ColorOutput "================================`n" "Cyan"

Write-Info "总检查项: $script:totalChecks"
Write-Success "通过: $script:passedChecks"

if ($script:failedChecks -gt 0) {
    Write-Error "失败: $script:failedChecks"
}

if ($script:warnings -gt 0) {
    Write-Warning "警告: $script:warnings"
}

$successRate = if ($script:totalChecks -gt 0) {
    [math]::Round(($script:passedChecks / $script:totalChecks) * 100, 1)
} else {
    0
}

Write-Info "成功率: $successRate%"

# 建议
Write-ColorOutput "`n================================" "Cyan"
Write-ColorOutput "建议" "Cyan"
Write-ColorOutput "================================`n" "Cyan"

if ($script:failedChecks -gt 0) {
    Write-Warning "发现 $script:failedChecks 个错误，建议："
    Write-Info "  1. 运行此脚本加 -FixIssues 参数尝试自动修复"
    Write-Info "  2. 检查 Git submodules: git submodule update --init --recursive"
    Write-Info "  3. 重新生成配置: cd emacs && make clean && make generate"
}

if ($script:warnings -gt 0) {
    Write-Warning "发现 $script:warnings 个警告，建议："
    Write-Info "  1. 查看上面的警告信息"
    Write-Info "  2. 如果正在进行重构，这些警告是正常的"
    Write-Info "  3. 完成重构后重新运行验证"
}

if ($script:failedChecks -eq 0 -and $script:warnings -eq 0) {
    Write-Success "`n✨ 配置验证通过！所有检查项正常。`n"
} else {
    Write-ColorOutput "`n需要注意以上问题。`n" "Yellow"
}

# 退出码
if ($script:failedChecks -gt 0) {
    exit 1
} else {
    exit 0
}
