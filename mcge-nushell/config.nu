$env.config.show_banner = false # 隐藏欢迎文本
$env.config.completions.external.max_results = 20
$env.config.shell_integration = true # VS Code 集成
$env.config.filesize.metric = true # 使用公制单位 1 KB = 1000 B、1 MB = 1e6 B 等

$env.config.history.max_size = 10000
$env.config.history.file_format = "sqlite" # 将历史记录保存到数据库，而不是 txt 文件

# alias
# alias gpl git pull
# alias gst git status
# alias gph git push
# alias gad git add .
# alias gsd git submodule update --init --recursive
# alias gsh git submodule foreach git reset --hard

def gad [file = "."] {
    git add $file
}

def gcm [commit_msg: string] {
    git commit -m $commit_msg
}

def gpl [] {
    git pull
}

def gph [] {
    git push
}

def gst [] {
    git status
}

def gir [] {
    git submodule update --init --recursive
}

def grh [] {
    git submodule foreach git reset --hard
}

def grs [file: string] {
    git restore --staged $file
}

def glg [] {
    git log
}

def grf [file: string] {
    git restore $file
}

use C:/Users/Administrator/.cache/starship/init.nu