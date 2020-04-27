unset MAILCHECK

# disable oh-my-zsh update prompts
DISABLE_UPDATE_PROMPT=true

# auto-install zplug
if [[ ! -d ~/.zplug ]]; then
    echo '[zshrc] installing zplug...'
    git clone https://github.com/b4b4r07/zplug ~/.zplug
fi

# load zplug and register / load plugins
if [[ -f ~/.zplug/init.zsh ]]; then
    source ~/.zplug/init.zsh
    zplug 'b4b4r07/zplug'

    zplug 'plugins/git', from:oh-my-zsh
    zplug 'hchbaw/opp.zsh', use:opp.zsh

    # define RPS1 in order to avoid the annoying vim status
    export RPS1=" "
    zplug 'plugins/vi-mode', from:oh-my-zsh

    # Liquid prompt
    LP_ENABLE_TIME=1
    LP_USER_ALWAYS=1
    zplug 'nojhan/liquidprompt'

    # Install / load plugins
    # note: install (new) plugins using 'zplug install'
    zplug load

    # Add <TAB> completion handlers for fzf *after* fzf is loaded
    _fzf_complete_z() {
      _fzf_complete '--multi --reverse' "$@" < <(raw_z)
    }
fi

# Delete to slashes via C-W
# Refer: http://unix.stackexchange.com/a/250700
my-backward-delete-word() {
    local WORDCHARS=${WORDCHARS/\//}
    zle backward-delete-word
}
zle -N my-backward-delete-word
bindkey '^W' my-backward-delete-word

# Use <C-P> and <C-N> to naviagte history
bindkey "^P" up-line-or-history
bindkey "^N" down-line-or-history

# Exports
export EDITOR='vim'
export KEYTIMEOUT=1
export DISABLE_AUTO_TITLE=true
export PATH="/usr/local/bin:/snap/bin:$PATH"
export PATH="~/.local/bin:$PATH"
export COLORTERM=xterm-256color

# Qutebrowser
export QT_AUTO_SCREEN_SCALE_FACTOR=2

# Aliases
alias ll='ls -lh'
alias _='sudo'
alias tmux='tmux -2'
alias emacs='TERM=xterm-256color emacs -nw'
alias cdu='cd-gitroot'

# FZF
if [[ -f ~/.fzf.zsh ]]; then
    # echo "[zshrc] loading fzf..."
    source ~/.fzf.zsh

    # Interactively select and activate a given docker-machine
    docker-activate () {
        input=$(docker-machine ls | tail -n+2 | fzf)
        if [[ $? = 0 ]]; then
            machine=$(echo "$input" | tr -s ' ' | cut -d' ' -f1,3)
            machine_name=$(echo "$machine" | cut -d' ' -f1)
            machine_status=$(echo "$machine" | cut -d' ' -f2)
            (if [[ $machine_status = "Stopped" ]]; then
                docker-machine start "$machine_name"
            fi || exit $?)
            echo "Activating docker machine '$machine_name'..."
            eval "$(docker-machine env $machine_name)"
        fi
    }

    # Interactively select and kill a running docker container
    select-adb-device () {
        input=$(adb devices | tail -n+2 | sed -e '$ d' | cut -f1 | fzf)
        if [[ $? = 0 ]]; then
            echo "selected adb device: ${input}"
            export ADB_DEVICE=$input
        fi
    }

    # fs [FUZZY PATTERN] - Select selected tmux session
    #   - Bypass fuzzy finder if there's only one match (--select-1)
    #   - Exit if there's no match (--exit-0)
    fs() {
      local session
      session=$(tmux list-sessions -F "#{session_name}" | \
        fzf --query="$1" --select-1 --exit-0) &&
      tmux switch-client -t "$session"
    }
fi

export FZF_COMPLETION_TRIGGER='**'
export FZF_TMUX=1
export FZF_DEFAULT_COMMAND='
  (git ls-files --cached --others --exclude-standard ||
   find . -path "*/\.*" -prune -o -type f -print -o -type l -print |
      sed s/^..//) 2> /dev/null'
export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"

# Set history options
# borrowed from: https://github.com/mattjj/my-oh-my-zsh/blob/master/history.zsh
HISTFILE="$HOME/.zhistory"
HISTSIZE=10000000
SAVEHIST=10000000

setopt BANG_HIST                 # Treat the '!' character specially during expansion.
setopt EXTENDED_HISTORY          # Write the history file in the ":start:elapsed;command" format.
setopt INC_APPEND_HISTORY        # Write to the history file immediately, not when the shell exits.
setopt SHARE_HISTORY             # Share history between all sessions.
setopt HIST_EXPIRE_DUPS_FIRST    # Expire duplicate entries first when trimming history.
setopt HIST_IGNORE_DUPS          # Don't record an entry that was just recorded again.
setopt HIST_IGNORE_ALL_DUPS      # Delete old recorded entry if new entry is a duplicate.
setopt HIST_FIND_NO_DUPS         # Do not display a line previously found.
setopt HIST_IGNORE_SPACE         # Don't record an entry starting with a space.
setopt HIST_SAVE_NO_DUPS         # Don't write duplicate entries in the history file.
setopt HIST_REDUCE_BLANKS        # Remove superfluous blanks before recording entry.
setopt HIST_VERIFY               # Don't execute immediately upon history expansion.
setopt HIST_BEEP                 # Beep when accessing nonexistent history.

# Node - Node.js
PATH="$PATH:.node/bin"

# Go - The go language
export GOPATH=~/go
export PATH="$PATH:$GOPATH/bin"
export PATH="/usr/local/bin:$PATH"
export PATH="$PATH:/usr/local/opt/go/libexec/bin"
export PATH="~/bin:$PATH"
export PATH="~/.local/bin:$PATH"

# Editor
function e { TERM=screen-256color emacsclient -nw "$@"; }

# Manpages
# Set case-insensitve searching on man pages
export MANPAGER='less -I'

# note: nvm (node version manager) is /very/ slow to start up.
#       it's faster to just put /some/ version of node onto the PATH and change
#       it when needed. To change it:
#       > source ~/.nvm/nvm.sh
#       > nvm use <version>
export PATH=$PATH:~/.nvm/versions/node/v8.5.0/bin

# Android dev
export ANDROID_HOME=~/Library/Android/sdk
export ANDROID_NDK_HOME=$ANDROID_HOME/ndk-bundle
export PATH=$PATH:${ANDROID_HOME}/tools
export PATH=$PATH:${ANDROID_HOME}/platform-tools
export PATH=$PATH:/usr/local/go/bin
export PATH=$PATH:~/.local/bin

# Explicitly set language
export LC_CTYPE=en_US.UTF-8
export LC_ALL=en_US.UTF-8


# Rust
PATH=~/.cargo/bin:$PATH

export precmd () {
	if [[ -n "$TMUX" ]]; then
		print -Pn "\e]0;tmux - $(tmux display -p '#S')\a"
	else
		# XXX assuming xterm here
		print -Pn "\e]0;xterm - %n@%m: %~\a"
	fi
}
export $(dbus-launch 2>/dev/null)

export configure_systemctl () {
	for e in \
		OAUTH_CLIENT_SECRET \
		OAUTH_REFRESH_TOKEN_felix \
		OAUTH_REFRESH_TOKEN_sylo; do
		env "$e"=$(pass "$e") systemctl --user import-environment "$e"
	done
}

eval $(keychain 2>/dev/null --eval)
