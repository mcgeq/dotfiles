if status is-interactive
    # Commands to run in interactive sessions can go here
end

# emacs
abbr e "emacs -nw"
set -x PATH $PATH "~/emacs/bin"
# 确保 Fish 启动时 PATH 包含 Cargo bin
set -gx PATH $HOME/.cargo/bin $PATH

# emacs end

# paru
abbr pry "paru -Syyu"

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
# paru end

# git
abbr gad  "git add ."
abbr gpl  "git pull"
abbr gplr "git pull --rebase"
abbr gph  "git push"
abbr gsi  "git submodule update --init --recursive"
abbr gsh  "git submodule foreach git reset --hard"
abbr gsm  "git submodule foreach 'git checkout (git symbolic-ref --short HEAD)'"
abbr gst  "git status"
# git end

# cargo bin
set -x PATH $PATH "~/.cargo/bin"
# cargo bin end


# starship
starship init fish | source

# pnpm
set -gx PNPM_HOME "/home/mcgeq/.local/share/pnpm"
if not string match -q -- $PNPM_HOME $PATH
  set -gx PATH "$PNPM_HOME" $PATH
end
# pnpm end

# 自动启动 tmux
if not set -q TMUX
  tmux new-session -A -D -s main
end
