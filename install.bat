@echo off
setlocal EnableDelayedExpansion

chcp 65001

REM 检查是否以管理员权限运行脚本
set userHome=%USERPROFILE%
set appDataRoaming=%APPDATA%
set localAppData=%LOCALAPPDATA%
set userName=%USERNAME%
set os=%OS%

set configHome=%cd%
set linkFile=%cd%\linkfile.txt
set dirOrFile=d

net session >nul 2>&1
if %errorLevel% == 0 (
    set "isAdmin=true"
) else (
    set "isAdmin=false"
)

if "%isAdmin%" == "true" (
    REM 管理员权限执行
    echo The BAT script is being executed with administrator privileges.
    
    for /f "tokens=*" %%i in ('type %1\linkfile.txt') do (
        set t=%%i
        set str=!t:USERPROFILE=%userHome%!
        set str=!str:APPDATA=%appDataRoaming%!
        set str=!str:DATALOCAL=%localAppData%!
    
        if "!str!" == "[FILE]" (
            set "dirOrFile=f"
        )

        if "!dirOrFile!" == "d" (
            if not "!str!" == "[DIR]" (
                for /f "tokens=1" %%A in ("!str!") do (
                    if not exist %%A (
                        mklink /d !str!
                    )
                )
            )
        )

        if "!dirOrFile!" == "f" (
            if not "!str!" == "[FILE]" (
                 for /f "tokens=1" %%A in ("!str!") do (
                    if not exist %%A (
                        mklink !str!
                    )
                )
            )
        )
    )
    pause
    exit /b
) else (
    echo Update the git project and update the child module.
    call :pullAndUpdateGitProject %configHome%
    call :traverseAndCheckGitProject %configHome%
    echo Request administrator privileges
    powershell -Command "Start-Process '%0' -Verb RunAs -ArgumentList '%configHome%'"
    exit /b
)

REM 定义函数: 拉取 Git 项目并更新子模块
:pullAndUpdateGitProject
cd /d "%1"
git pull
if exist ".gitmodules" (
    echo Start the update submodule...
    git submodule update --init --recursive
) else (
    echo You don't need to update submodule.
)
exit /b

REM 定义函数: 遍历目录并判断是否是 Git 项目
:traverseAndCheckGitProject
    cd /d %1
    for /f "tokens=2" %%a in ('git submodule status') do (
        cd /d %cd%\%%a
        call :switch_to_main_branch %cd%
    )
exit /b

REM 检查切换git为主分支
:switch_to_main_branch
    REM 切换到主仓库目录
    echo "checkout ... " %cd% " ... branch"
    REM 获取远程分支列表
    for /f "tokens=*" %%l in ('git branch -r') do (
        set "branch=%%l"
        if "!branch!" == "origin/main" (
            git checkout main
            git pull
        )

        if "!branch!" == "origin/master" (
            git checkout master
            git pull
        )
    )
    call :traverseAndCheckGitProject %cd%
exit /b

REM 定义函数: 判断目录是否是Git项目
:isGitProject
cd /d "%1"
if exist ".git" (
    call :switch_to_main_branch %cd%
    exit /b 0
) else (
    exit /b 1
)
