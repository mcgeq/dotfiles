# config.nu
#
# Installed by:
# version = "0.106.1"
#
# This file is used to override default Nushell settings, define
# (or import) custom commands, or run any other startup tasks.
# See https://www.nushell.sh/book/configuration.html
#
# Nushell sets "sensible defaults" for most configuration settings, 
# so your `config.nu` only needs to override these defaults if desired.
#
# You can open this file in your default editor using:
#     config nu
#
# You can also pretty-print and page through the documentation for configuration
# options using:
#     config nu --doc | nu-highlight | less -R

# For more information on defining custom themes, see
# https://www.nushell.sh/book/coloring_and_theming.html
# And here is the theme collection
# https://github.com/nushell/nu_scripts/tree/main/themes

# Custom Git aliases and commands
# These replace the old commented alias syntax with proper Nushell function definitions

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

# Jujutsu (jj) 工具函数
def jjInit [] {
    jj init
}

def jjAdd [] {
    jj add
}

def jjCommit [message: string] {
    jj commit -m $message
}

def jlg [] {
    jj log
}

def jDif [] {
    jj diff
}

def jst [] {
    jj status
}

def jjCheckout [revision: string] {
    jj checkout $revision
}

def jjMerge [revision: string] {
    jj merge $revision
}

# Initialize oh-my-posh prompt if available
# 设置主题路径
let posh_theme = "D:/config/dotfiles/ohmyposh"

# 正确的主提示符配置（单行写法）
$env.PROMPT_COMMAND = { oh-my-posh print primary --config $"($posh_theme)/mcgeq.omp.toml" --shell nushell }

# 或者多行写法（使用括号包裹）
$env.PROMPT_COMMAND = {(
    oh-my-posh print primary
        --config $"($posh_theme)/mcgeq.omp.toml"
        --shell nushell
)}

# 可选的右侧提示符
$env.PROMPT_COMMAND_RIGHT = { oh-my-posh print right --config $"($posh_theme)/mcgeq.omp.toml" --shell nushell }

# 提示符指示器
$env.PROMPT_INDICATOR = $"(ansi green)〉(ansi reset) "