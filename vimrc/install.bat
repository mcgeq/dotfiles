@echo off
setlocal enabledelayedexpansion

REM ---------------------------------------------------------------------------
REM 定位路径
REM ---------------------------------------------------------------------------
set "TARGET_DIR=%USERPROFILE%\vimfiles"
set "WRITE_VIMRC_FILE=%TARGET_DIR%\vimrc"
set "SOURCE_FILE=%cd%\vimrc.txt"

echo Target vimrc will be written to: "%WRITE_VIMRC_FILE%"
echo Source vimrc: "%SOURCE_FILE%"

REM ---------------------------------------------------------------------------
REM 校验路径
REM ---------------------------------------------------------------------------
if not exist "%TARGET_DIR%" (
    echo Target directory not found. Creating "%TARGET_DIR%" ...
    mkdir "%TARGET_DIR%" || (
        echo Failed to create target directory. Abort.
        exit /b 1
    )
)

if not exist "%SOURCE_FILE%" (
    echo Source file not found. Abort.
    exit /b 1
)

REM ---------------------------------------------------------------------------
REM 备份原 vimrc
REM ---------------------------------------------------------------------------
for /f "tokens=1-3 delims=/-. " %%a in ("%date%") do (
    set dateStamp=%%c-%%a-%%b
)

for %%F in ("%WRITE_VIMRC_FILE%") do (
    set fileName=%%~nF
    set fileExtension=%%~xF
    set sourceFolder=%%~dpF
)

set "distFile=%sourceFolder%%fileName%_%dateStamp%%fileExtension%"

if exist "%WRITE_VIMRC_FILE%" (
    copy /y "%WRITE_VIMRC_FILE%" "%distFile%" >nul && (
        echo Backup saved to "%distFile%"
    ) || (
        echo Failed to backup existing vimrc. Abort.
        exit /b 1
    )
    del /q "%WRITE_VIMRC_FILE%"
)

REM ---------------------------------------------------------------------------
REM 写入新的 vimrc
REM ---------------------------------------------------------------------------
type "%SOURCE_FILE%" > "%WRITE_VIMRC_FILE%"
if errorlevel 1 (
    echo Failed to write vimrc.
    exit /b 1
)

echo File written successfully.
exit /b 0
