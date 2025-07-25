#!/usr/bin/env bash

GitGlobalDir=$HOME/mcge-gitconfig
GitGlobalConfigFilePath=$GitGlobalDir/gitconfig
GitGlobalCommitFilePath=$GitGlobalDir/commit.txt
GitGlobalHooksDir=$GitGlobalDir/hooks

# 获取当前shell
# current_use_shell=$(basename "$SHELL")
# if [ "$current_use_shell" = "bash" ] || [ "$current_use_shell" = "zsh" ]; then
#    export GIT_CONFIG_GLOBAL=$GitGlobalConfigFilePath
# elif [ "$current_use_shell" = "fish" ]; then
#    set -x GIT_CONFIG_GLOBAL $GitGlobalConfigFilePath
# else
#    echo "For the current shell [ $current_use_shell ]. Set the value of the GIT_CONFIG_GLOBAL environment variable to [ $GitGlobalCommitFilePath ]"
# fi


# echo The setup was successful. Set up a global configuration file: [ $GitGlobalConfigFilePath ]

while read line; do
    if [[ $line =~ ^\[.*\]$ ]]; then
        newLine=$line
        newLine=${newLine:1:-1}
        if [[ $newLine =~ \" ]]; then
            newLineArray=$(echo $newLine | tr " " "\n")
            newLine=""
            for v in ${newLineArray[@]}; do
                if [[ $v =~ ^\".*\"$ ]]; then
                    nv="${v:1:-1}"
                    if [[ $newLine == "" ]]; then
                        newLine="$nv"
                    else
                        newLine="$newLine.$nv"
                    fi
                else
                    if [[ $newLine == "" ]]; then
                        newLine="$v"
                    else
                        newLine="$newLine.$v"
                    fi
                fi
            done
        fi
    else
        leftV=${line%=*}
        rightV=${line#*=}
        if [[ $leftV =~ "autocrlf" ]]; then
            rightV="input"
        fi
        git config --global $newLine.$leftV $rightV
        echo Set the git global property [ $newLine.$leftV ] to [ $rightV ].
    fi
done < $GitGlobalConfigFilePath

# echo Set up a git global commit template.

git config --global commit.template $GitGlobalCommitFilePath

echo The setup was successful. Set up a global commit template: [ $GitGlobalCommitFilePath ]

git config --global core.hooksPath $GitGlobalHooksDir

echo The setup was successful. Set up a global hooks directory: [ $GitGlobalHooksDir ]

chmod ug+x $GitGlobalHooksDir/*
