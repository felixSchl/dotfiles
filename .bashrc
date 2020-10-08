# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

# If not running interactively, don't do anything
[ -z "$PS1" ] && return

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
if [ -z "$debian_chroot" ] && [ -r /etc/debian_chroot ]; then
    debian_chroot=$(cat /etc/debian_chroot)
fi

# ---------------------------------------------------------------------------- #
# ALIASES
# ---------------------------------------------------------------------------- #

# Set OS-specific aliases.
case `uname` in
    Darwin)
        alias ls='ls -h'
        alias emacs='/Applications/Emacs.app/Contents/MacOS/Emacs'
        ;;
    Linux)
        alias ls='ls --color=auto --human-readable --ignore-backups'
        ;;
esac

# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls --color=auto'
    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi

alias gvi='google docs edit $1'
alias glist='google docs list'
alias ll='ls -alF'
alias la='ls -A'
alias l='ls -CF'

# Add an "alert" alias for long running commands.  Use like so:
#   sleep 10; alert
alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'

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
if [ -f /etc/bash_completion ] && ! shopt -oq posix; then
    . /etc/bash_completion
fi

alias gvi='google docs edit $1'
alias glist='google docs list'

# ---------------------------------------------------------------------------- #
# GIT ALIASES
# ---------------------------------------------------------------------------- #

function git_current_branch() {
  git symbolic-ref HEAD 2> /dev/null | sed -e 's/refs\/heads\///'
}

alias gse='git send-email'
alias gup='git fetch origin && gr origin/$(git_current_branch)'
alias grh='git reset --hard'
alias gb='git branch -v'
alias gbdl='git branch -d'
alias gbdr='git push origin --delete'
alias ga='git add'
alias gau='git add --update'
alias gs='git show'
alias gstat='git status'
alias grm='git rm'
alias gmv='git mv'
alias s='git status --short'
alias gcm='git commit -m'
alias gd='git diff'
alias gco='git checkout'
alias gbrt='~/environment/bin/gbrt.rb'
alias gr='git rebase --preserve-merges'
alias gm='git merge --no-ff'
alias gref='git reflog'
alias glp='git log --patch'
alias gls='git log -S'
alias gsp='git smart-pull'
alias gp='git push origin HEAD:$(git_current_branch)'
alias gf='git fetch'
alias gc='git clean'
alias gclone='git clone'
alias gl='git log --graph --all --format=format:"%C(bold blue)%h%C(reset) %C(green)- %s %C(reset)%C(bold green)— %an%C(reset) %C(cyan)(%ar)%C(reset) %C(blue)%d%C(reset)" --abbrev-commit --date=relative'

# ---------------------------------------------------------------------------- #
# SHELL COLOURS
# ---------------------------------------------------------------------------- #

# Solarize
function dark {
    ~/environment/bin/solarize.sh dark
}
function light {
    ~/environment/bin/solarize.sh light
}

# Use ls colors scheme supported by this OS.
case `uname` in
    Linux)  eval "$(dircolors -b)";;
    *)      export CLICOLOR=1;;
esac

# ---------------------------------------------------------------------------- #
# CUSTOMISE PROMPT
# ---------------------------------------------------------------------------- #

# Red if last command failed
function __last_cmd_code_ps1() {
        local red='\033[00;31m' # red
 
        if [ $1 -ne 0 ]; then
                echo -e "${red}*!!**${1}**!!*${red}"
                return
        fi
}
 
function prompt() {
        local nc='\033[0m' # no color
        local green='\033[00;32m'
        local purple='\033[01;35m'
        local cyan='\033[00;36m'
local ps1=$(echo $cyan''$cyan'\w'$purple'$(__git_ps1 " (%s)" 2>/dev/null)')
PS1=$(echo "$ps1""$nc""
>>> ");
}
prompt

# ---------------------------------------------------------------------------- #
# TOOLS IN PATH ETC.
# ---------------------------------------------------------------------------- #

PATH=$PATH:$HOME/.rvm/bin # Add RVM to PATH for scripting

[ -f ~/.fzf.bash ] && source ~/.fzf.bash

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion
