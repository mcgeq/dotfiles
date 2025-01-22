if status is-interactive
    # Commands to run in interactive sessions can go here
end

# 定义一个显示欢迎语的函数
function show_welcome
    # 获取 Git 配置中的用户名，如果没有则使用默认值
    set -l git_user_name (git config --get user.name ^/dev/null)
    if test -z "$git_user_name"
        set git_user_name "mcgeq"
    end

    # 获取当前时间
    set hour (date +'%H')
    set current_time (date '+%Y-%m-%d %H:%M:%S')

    # 打印英文问候语
    if test $hour -lt 12
        echo "$current_time Good morning, $git_user_name!"
    else if test $hour -lt 18
        echo "$current_time Good afternoon, $git_user_name!"
    else
        echo "$current_time Good evening, $git_user_name!"
    end
end

show_welcome

# emacs
abbr e "emacs -nw"
set -x PATH $PATH "~/emacs/bin"

# emacs end

# git
abbr gpl  "git pull"
abbr gplr "git pull --rebase"
abbr gph  "git push"
abbr gsi  "git submodule update --init --recursive"
abbr gsh  "git submodule foreach git reset --hard"
abbr gsm  "git submodule foreach 'git checkout $(git symbolic-ref --short HEAD)'"
abbr gst  "git status"
# git end

# paru -Ss
function prs
    set package_name $argv[1]
    set limit_value $argv[2]  # 如果没有传递 limit 参数，默认为 5
    # 确保 limit_value 非空，如果为空则设置为 5
    if test -z $limit_value
        set limit_value 5
    end
    paru -Ss $package_name --limit $limit_value
end


# paru -S
function pri
  if not count $argv
    echo "Error: You must provide at least one package to install."
    return 1
    end
    paru -S $argv
end

# paru -Rns
function prr
  if not count $argv
    echo "Error: You must provide at least on package to remove."
    return 1
    end
    paru -Rns $argv
end

# starship
starship init fish | source

# pnpm
set -gx PNPM_HOME "/home/mcgeq/.local/share/pnpm"
if not string match -q -- $PNPM_HOME $PATH
  set -gx PATH "$PNPM_HOME" $PATH
end
# pnpm end