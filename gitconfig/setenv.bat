@echo off
setlocal EnableDelayedExpansion

set GitGlobalDir=D:/config/dotfiles/mcge-gitconfig
set GitGlobalConfigFilePath=%GitGlobalDir%/gitconfig
set GitGlobalCommitFilePath=%GitGlobalDir%/commit.txt
set GitGlobalHooksDir=%GitGlobalDir%/hooks

REM setx GIT_CONFIG_GLOBAL %GitGlobalConfigFilePath%

REM  echo The setup was successful. Set up a global git configuration file: [ %GitGlobalConfigFilePath% ]

REM echo Set up a git global commit commit template.

git config --global commit.template %GitGlobalCommitFilePath%

echo The setup was successful. Set up a global commit commit template: [ %GitGlobalCommitFilePath%]

git config --global core.hooksPath %GitGlobalHooksDir%

echo The setup was successful. Set up a global hooks directory : [ %GitGlobalHooksDir% ]

for /f "usebackq delims=" %%a in (%GitGlobalConfigFilePath%) do (
    REM 判断
    set "line=%%a"
    if "!line:~0,1!"=="[" (
        if "!line:~-1!"=="]" (
            set "section=!line:~1,-1!"
        )
        if "!section!" neq "!section: =!" (
            for /f "tokens=1,2 delims= " %%g in ("!section!") do (
                set "p1=%%h"
                set "p2=!p1:~1,-1!"
                set "section=%%g.!p2!"
            )
        )
    )
    if not "!line:~0,1!"=="[" (
        REM echo Section: !section!
        REM 去除行首尾空白字符串
        for /f "tokens=*" %%b in ("!line!") do (
            set "line=%%b"
        )

        REM 按 = 分割，并去除首尾空白字符
        for /f "tokens=1,2 delims==" %%c in ("!line!") do (
            set "ky=%%c"
            set "kv=%%d"
        )

        REM 去除空白字符
        for /f "tokens=*" %%e in ("!ky!") do (
            set "ky=%%e"
        )

        for /f "tokens=*" %%f in ("!kv!") do (
            set "kv=%%f"
        )
        git config --global !section!.!ky! !kv!
        echo Set the git global property [ !section!.!ky! ] to [ !kv! ].
    )
)

endlocal
