@echo off
setlocal enabledelayedexpansion

REM 文件存在会覆盖文件内容
set WRITE_VIMRC_FILE=%USERPROFILE%\vimfiles\vimrc

echo The path to file written is [ %WRITE_VIMRC_FILE% ]

REM 获取当前日期
set dateStamp=%date:~0,4%-%date:~5,2%-%date:~8,2%

REM 获取源文件的文件名(不包括扩展名)
for %%F in ("%WRITE_VIMRC_FILE%") do (
    set fileName=%%~nF
    set fileExtension=%%~xF
    set sourceFolder=%%~dpF
)

set "distFile=%sourceFolder%%fileName%_%dateStamp%%fileExtension%"

if exist %WRITE_VIMRC_FILE% (
    REM 复制文件并重命名
    copy ^/y "%WRITE_VIMRC_FILE%" "%distFile%"
    
    echo The file is copied as [ %distFile% ]

    del %WRITE_VIMRC_FILE%
)

REM 获取当前目录
set currentDirPath=%cd%
set vimrcFilePath=%currentDirPath%\vimrc.txt

echo Read file is [ %vimrcFilePath% ]

type %vimrcFilePath% > %WRITE_VIMRC_FILE%

echo File write successfully.
