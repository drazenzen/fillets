# .bashrc

# Source global definitions
if [ -f /etc/bashrc ]; then
    . /etc/bashrc
fi

# User specific environment
if ! [[ "$PATH" =~ "$HOME/.local/bin:$HOME/bin:" ]]
then
    PATH="$HOME/.local/bin:$HOME/bin:$PATH"
fi

# User specific aliases and functions
umask 0022

HISTCONTROL=ignoreboth # Don't put duplicate lines or lines starting with space in the history
shopt -s histappend
shopt -s cmdhist
HISTSIZE=10000
HISTFILESIZE=10000

shopt -s checkwinsize  # check window size after each command

# Turn off XON/XOFF
# Used to pause the sending of data to console with CTRL-S and enabling it again with CTRL-Q)
stty -ixon

# Ref: http://unix.stackexchange.com/questions/66581
# For getting both the exit status and output from command

# HG prompt
if ! command -v hg &> /dev/null; then
    echo "Mercurial not found"
fi
__hg_ps1() {
    local INFO
    INFO=$(hg branch 2> /dev/null)
    if [ $? -eq 0 ]; then
        echo -e " (HG: $INFO $(hg status | cut -b 1 | uniq | sort | tr -d '\n'))"
    fi
}
# Ref: http://gilesorr.com/bashprompt/prompts/jobs.html
__jobcount() {
    stopped="$(jobs -s | wc -l | tr -d " ")"
    running="$(jobs -r | wc -l | tr -d " ")"
    if [ "$stopped" != 0 -o "$running" != 0 ]; then
        echo -n " [${running}r/${stopped}s]"
    fi
}
# Processes
__processcount() {
    echo -e "[p=$(ps ux | wc -l)]"
}
# Screen
if ! command -v screen &> /dev/null; then
    echo "Screen not found"
fi
__screen() {
    if ! screen -Ux;
    then
        echo "Running screen session found..."
        screen -U
    fi
}
# Term title
__termtitle() { printf "\033]0;$*\007"; }
# Docker compose exec
__docker_compose_exec() {
    __termtitle "$1"
    docker-compose exec "$1" bash
}

# Git prompt
if ! command -v git &> /dev/null; then
    echo "Git not found"
fi
if [ -f $HOME/.git-prompt.sh ]; then
    source $HOME/.git-prompt.sh
    GIT_PS1_SHOWDIRTYSTATE=1
else
    echo "Git prompt script not found"
fi

# Dir env
if ! command -v direnv &> /dev/null; then
    echo "Direnv not found"
fi
eval "$(direnv hook bash)"

# Terraform env
if [ -f $HOME/apps/tfenv/bin/tfenv ]; then
    PATH="$HOME/apps/tfenv/bin/:$PATH"
fi

# Colorful GCC output
export GCC_COLORS='error=01;31:warning=01;35:note=01;36:caret=01;32:locus=01:quote=01'

# Python
export PYTHONSTARTUP=$HOME/.pythonrc

# Node.js
if [ -d "$HOME/.local/node" ]; then
    export NODEJS_HOME=$HOME/.local/node
    PATH=$NODEJS_HOME/bin:$PATH
else
    echo "Node.js directory not found"
fi

# SciTE
if command -v SciTE &> /dev/null; then
    mkdir -p $HOME/.config/scite
    export SciTE_USERHOME=$HOME/.config/scite
else
    echo "SciTE not found"
fi

# Aliases
alias ll='ls -al'
alias la='ls -A'
alias l='ls -CF'
alias g='grep'
alias e='vim'
alias py='python'
alias ..='cd ..'
alias ...='cd ../..'
alias sc=__screen
alias black-power='git diff --name-only $(git merge-base master HEAD) | grep py$ | sed "s|.*|$(git rev-parse --show-toplevel)/\0|" | xargs -t -n1 black --config $(git rev-parse --show-toplevel)/pyproject.toml'
alias kubedashtoken='kubectl get secret -n kube-system $(kubectl -n kube-system get secret | grep eks-admin | awk '\''{print $1}'\'') -o json | jq -r ".data.token" | base64 -d'
alias de=__docker_compose_exec

# Fortunes
if [ -f $HOME/bin/fortune ]; then
    $HOME/bin/fortune
else
    echo "Fortune not found"
fi

PS1='[\u@\h \W$(__hg_ps1)$(__git_ps1 " (%s)")]\$ '
PROMPT_COMMAND="$PROMPT_COMMAND; history -a"
export PATH

# vim:ts=4:et
