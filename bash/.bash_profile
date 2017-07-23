#!/usr/bin/env bash

# Path to the bash it configuration
export BASH_IT=~/.bash_it

# Set bash-it theme
export LP_ENABLE_TIME=1
export LP_USER_ALWAYS=1
export LP_ENABLE_BATT=0
export LP_ENABLE_LOAD=0
export BASH_IT_THEME=liquidprompt

# Configure projects
export PROJECT_PATHS=~/projects:~/projects/dn3010

# Don't check mail at startup
unset MAILCHECK

# Settings
export MANPAGER='less -I'
export IRC_CLIENT=irssi
export SCM_CHECK=true
export PATH=/usr/local/bin:$PATH
export LC_CTYPE=en_US.UTF-8
export LC_ALL=en_US.UTF-8

# Load Bash It
source "$BASH_IT"/bash_it.sh

# Enable bash-it plugins
bashit > /dev/null disable plugin all
bashit > /dev/null enable plugin \
	alias-completion \
	base \
	edit-mode-vi \
	fzf \
	hub \
	projects \
	tmux

# Fix navigation in-vim insert mode
bind -m vi-insert "\C-l":clear-screen
bind -m vi-insert "\C-p":history-search-backward
bind -m vi-insert "\C-n":history-search-forward
stty werase undef
bind "\C-w":unix-filename-rubout

# Android dev
export ANDROID_HOME=~/Library/Android/sdk
export ANDROID_NDK_HOME=$ANDROID_HOME/ndk-bundle
export PATH=$PATH:${ANDROID_HOME}/tools
export PATH=$PATH:${ANDROID_HOME}/platform-tools

export FZF_COMPLETION_TRIGGER='**'
# export FZF_DEFAULT_COMMAND='
#   (git ls-files --cached --others --exclude-standard ||
#    find . -path "*/\.*" -prune -o -type f -print -o -type l -print |
#       sed s/^..//) 2> /dev/null'
export FZF_CTRL_T_COMMAND=$FZF_DEFAULT_COMMAND

# Less syntax highlighting
# install on OSX: brew install source-highlight
if [ "$(which src-hilite-lesspipe.sh)" ]; then
	export LESSOPEN='| src-hilite-lesspipe.sh %s'
	export LESS=' -R '
fi
