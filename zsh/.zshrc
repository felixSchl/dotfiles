raw_z () {
  echo "$(z -e | tail | awk '{ print $2 }')"
}

fzf_z () {
    dir=$(raw_z | fzf)
    if [[ $? = 0 ]]; then
        cd "$dir"
    fi
}

if [[ -f ~/.zplug/zplug ]]; then
    source ~/.zplug/zplug
    zplug "b4b4r07/zplug"

    # Triaging
    zplug "tarruda/zsh-autosuggestions"
    zplug "zsh-users/zsh-history-substring-search"
    zplug "zsh-users/zsh-completions"

    # Syntax highlighting
    # zplug "zsh-users/zsh-syntax-highlighting"

    # Completions etc.
    zplug "plugins/git", from:oh-my-zsh

    # VIM key-mappings for zsh
    zplug "plugins/vi-mode", from:oh-my-zsh
    zplug "hchbaw/opp.zsh", of:opp.zsh
    bindkey -M vicmd 'k' history-substring-search-up
    bindkey -M vicmd 'j' history-substring-search-down

    # Z - jump to recent directories
    zplug "rupa/z", of:z.sh

    # Liquid prompt
    LP_ENABLE_TIME=1
    LP_USER_ALWAYS=1
    zplug "nojhan/liquidprompt"

    # Navigate to the .git project root
    zplug "mollifier/cd-gitroot"
    alias cdu='cd-gitroot'

    # Install / load plugins
    zplug check || zplug install
    zplug load

    # Bind <C-K> to `z`
    bindkey -s '^k' '^qfzf_z\n'

    # Add <TAB> completion handlers for fzf *after* fzf is loaded
    _fzf_complete_z() {
      _fzf_complete "--multi --reverse" "$@" < <(raw_z)
    }
fi

# Exports
export EDITOR='vim'
export KEYTIMEOUT=1
export DISABLE_AUTO_TITLE=true
export PATH="/usr/local/bin:$PATH"

# Aliases
alias _='sudo'
alias tmux='tmux -2'
alias emacs='TERM=xterm-256color emacs -nw'
alias cdu='cd-gitroot'
if type rlwrap > /dev/null; then
    alias node='rlwrap node'
fi

# FZF
if [[ -f ~/.fzf.zsh ]]; then
    echo "Loading fzf..."
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
fi

export FZF_COMPLETION_TRIGGER='**'
export FZF_TMUX=1
export FZF_DEFAULT_COMMAND='
  (git ls-tree -r --name-only HEAD ||
   find . -path "*/\.*" -prune -o -type f -print -o -type l -print |
      sed s/^..//) 2> /dev/null'

# Node - Node.js
PATH="$PATH:.node/bin"

# Go - The go language
export GOPATH=~/go
export PATH="$PATH:$GOPATH/bin"
export PATH="/usr/local/bin:$PATH"
export PATH="$PATH:/usr/local/opt/go/libexec/bin"

# Nvm - Node version manager
export NVM_DIR=~/.nvm
if [[ -f ~/.nvm/nvm.sh ]]; then
    echo "Loading nvm..."
    source ~/.nvm/nvm.sh
    nvm alias default 0.12
    nvm use default
fi

# Fuck - command correction
if type thefuck > /dev/null; then
    echo "Loading thefuck..."
    eval $(thefuck --alias)
fi

# Explicitely set language
export LC_CTYPE=en_US.UTF-8
export LC_ALL=en_US.UTF-8

# weechat
# TODO: Find a more sophisticated way...
if [[ ! -f ~/.weechat/python/autoload/wee_slack.py ]]; then
    echo "weechat: installing \`wee_slack.py\`..."
    mkdir -p ~/.weechat/python/autoload
    curl -fLo ~/.weechat/python/autoload/wee_slack.py \
        https://raw.githubusercontent.com/rawdigits/wee-slack/master/wee_slack.py \
        2> /dev/null
fi

clear
