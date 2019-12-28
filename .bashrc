# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

# If not running interactively, don't do anything
case $- in
    *i*) ;;
      *) return;;
esac

# don't put duplicate lines or lines starting with space in the history.
# See bash(1) for more options
HISTCONTROL=ignoreboth

# append to the history file, don't overwrite it
shopt -s histappend

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=1000
HISTFILESIZE=2000

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# If set, the pattern "**" used in a pathname expansion context will
# match all files and zero or more directories and subdirectories.
#shopt -s globstar

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# set variable identifying the chroot you work in (used in the prompt below)
if [ -z "${debian_chroot:-}" ] && [ -r /etc/debian_chroot ]; then
    debian_chroot=$(cat /etc/debian_chroot)
fi

# set a fancy prompt (non-color, unless we know we "want" color)
case "$TERM" in
    xterm-color|*-256color) color_prompt=yes;;
esac

# uncomment for a colored prompt, if the terminal has the capability; turned
# off by default to not distract the user: the focus in a terminal window
# should be on the output of commands, not on the prompt
#force_color_prompt=yes

if [ -n "$force_color_prompt" ]; then
    if [ -x /usr/bin/tput ] && tput setaf 1 >&/dev/null; then
	# We have color support; assume it's compliant with Ecma-48
	# (ISO/IEC-6429). (Lack of such support is extremely rare, and such
	# a case would tend to support setf rather than setaf.)
	color_prompt=yes
    else
	color_prompt=
    fi
fi

if [ "$color_prompt" = yes ]; then
    PS1='${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$ '
else
    PS1='${debian_chroot:+($debian_chroot)}\u@\h:\w\$ '
fi
unset color_prompt force_color_prompt

# If this is an xterm set the title to user@host:dir
case "$TERM" in
xterm*|rxvt*)
    PS1="\[\e]0;${debian_chroot:+($debian_chroot)}\u@\h: \w\a\]$PS1"
    ;;
*)
    ;;
esac

# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls --color=auto'
    #alias dir='dir --color=auto'
    #alias vdir='vdir --color=auto'

    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi

# colored GCC warnings and errors
export GCC_COLORS='error=01;31:warning=01;35:note=01;36:caret=01;32:locus=01:quote=01'

# some more ls aliases
alias ll='ls -l'
alias la='ls -A'
alias l='ls -CF'

alias h='history'
alias g='grep'
alias e='vim'
alias py='python'
alias m='./manage.py'
alias ..='cd ..'
alias ...='cd ../..'

# Ref: http://unix.stackexchange.com/questions/66581
# For getting both the exit status and output from command
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
# processes
__processcount() {
    echo -e "[p=$(ps ux | wc -l)]"
}
# screen
__screen() {
    if ! screen -Ux;
    then
        echo "Running screen..."
        screen -U
    fi
}
alias sc=__screen

# git
if [ -f $HOME/.git-prompt.sh ]; then
    source $HOME/.git-prompt.sh
    GIT_PS1_SHOWDIRTYSTATE=1
else
    echo "Git prompt script not found"
fi

# Prompt
PS1='[\u@\[\033[0;32m\]\h\[\033[0m\] \W$(__hg_ps1)$(__git_ps1 " (%s)")]\$ '

# Alias definitions.
# You may want to put all your additions into a separate file like
# ~/.bash_aliases, instead of adding them here directly.
# See /usr/share/doc/bash-doc/examples in the bash-doc package.

if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if ! shopt -oq posix; then
  if [ -f /usr/share/bash-completion/bash_completion ]; then
    . /usr/share/bash-completion/bash_completion
  elif [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
  fi
fi

# 256 color support for Xfce4 Terminal
# http://stackoverflow.com/questions/19327836
if [ "$COLORTERM" == "xfce4-terminal" ] ; then
    export TERM=xterm-256color
fi

# PATH=$HOME/bin:$PATH
# turn off XOFF (stops commands from being received) CTRL-S
# to enable again commands use CTRL-Q
# bind -r '\C-s'
stty -ixon

# Python
export PYTHONSTARTUP=$HOME/.pythonrc

# Fortunes
if [ -f $HOME/bin/fortune ]; then
    $HOME/bin/fortune
else
    echo "Fortune not found"
fi

# vim:ts=4:et
