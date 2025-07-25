#!/bin/bash

# 设置写入的文件路径，存在则覆盖文件内容
WRITE_VIMRC_FILE="$HOME/.vimrc"

echo "The path to file written is [ $WRITE_VIMRC_FILE ]"

# 获取当前日期
dateStamp=$(date +"%Y-%m-%d")

# 获取源文件的文件名(不包括扩展名)
fileName=$(basename $WRITE_VIMRC_FILE)
fileExtension="${fileName##.}"
fileName="${fileName%.*}"
sourceFolder=$(dirname $WRITE_VIMRC_FILE)

distFile="$sourceFolder/$fileName"_"$dateStamp.$fileExtension"

if [ -f "$WRITE_VIMRC_FILE" ]; then
    # 复制文件并重命名
    cp -f "$WRITE_VIMRC_FILE" "$distFile"
    
    echo "The file is copied as [ $distFile ]"

    rm "$WRITE_VIMRC_FILE"
fi

# 获取当前目录
currentDirPath=$(pwd)
vimrcFilePath="$currentDirPath/vimrc.txt"

echo "Read file is [ $vimrcFilePath ]"

# 将文件内容写入指定文件
cat "$vimrcFilePath" > "$WRITE_VIMRC_FILE"

echo "File write successfully."
