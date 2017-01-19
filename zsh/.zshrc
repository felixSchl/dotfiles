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

    zplug 'zsh-users/zsh-syntax-highlighting'
    zplug 'plugins/git', from:oh-my-zsh
    zplug 'plugins/vi-mode', from:oh-my-zsh
    zplug 'hchbaw/opp.zsh', use:opp.zsh

    bindkey -M vicmd 'k' history-substring-search-up
    bindkey -M vicmd 'j' history-substring-search-down

    # Liquid prompt
    LP_ENABLE_TIME=1
    LP_USER_ALWAYS=1
    zplug 'nojhan/liquidprompt'

    # Navigate to the .git project root
    zplug 'mollifier/cd-gitroot'
    alias cdu='cd-gitroot'

    # Install / load plugins
    zplug check || zplug install
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
export PATH="/usr/local/bin:$PATH"
export COLORTERM=xterm-256color

# Aliases
alias ll='ls -lh'
alias _='sudo'
alias tmux='tmux -2'
alias emacs='TERM=xterm-256color emacs -nw'
alias cdu='cd-gitroot'
if type rlwrap > /dev/null; then
    alias node='rlwrap node'
fi

# Less syntax highlighting
# install on OSX: brew install source-highlight
if [ "$(which src-hilite-lesspipe.sh)" ]; then
    export LESSOPEN='| src-hilite-lesspipe.sh %s'
    export LESS=' -R '
fi

# FZF
if [[ -f ~/.fzf.zsh ]]; then
    echo "[zshrc] loading fzf..."
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
    docker-kill () {
        input=$(docker ps | tail -n+2 | fzf)
        if [[ $? = 0 ]]; then
            container=$(echo "$input" | cut -d' ' -f1)
            echo "Killing container '$container'..."
            docker kill "$container"
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

# Manpages
# Set case-insensitve searching on man pages
export MANPAGER='less -I'

# Nvm - Node version manager
export NVM_DIR=~/.nvm
if [[ -f ~/.nvm/nvm.sh ]]; then
    echo '[zshrc] loading nvm...'
    source ~/.nvm/nvm.sh
    # nvm alias default 4 > /dev/null
    # nvm use default     > /dev/null
fi

# Explicitely set language
export LC_CTYPE=en_US.UTF-8
export LC_ALL=en_US.UTF-8

# weechat
# TODO: Find a more sophisticated way...
if [[ ! -f ~/.weechat/python/autoload/wee_slack.py ]]; then
    echo '[zshrc] weechat: installing "wee_slack.py"...'
    mkdir -p ~/.weechat/python/autoload
    curl -fLo ~/.weechat/python/autoload/wee_slack.py \
        https://raw.githubusercontent.com/rawdigits/wee-slack/master/wee_slack.py \
        2> /dev/null
fi
