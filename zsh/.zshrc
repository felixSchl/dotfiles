if [[ -f ~/.zplug/zplug ]]; then
    source ~/.zplug/zplug
    zplug "b4b4r07/zplug"

    # Triaging
    zplug "b5b4r07/enhancd", at:v1, of:enhancd.sh
    zplug "tarruda/zsh-autosuggestions"
    zplug "uvaes/fzf-marks"

    zplug "zsh-users/zsh-syntax-highlighting"
    zplug "zsh-users/zsh-history-substring-search"

    # Completions etc.
    zplug "plugins/git", from:oh-my-zsh

    # VIM key-mappings for zsh
    zplug "plugins/vi-mode", from:oh-my-zsh
    zplug "hchbaw/opp.zsh", of:opp.zsh
    bindkey -M vicmd 'k' history-substring-search-up
    bindkey -M vicmd 'j' history-substring-search-down

    # Liquid prompt
    LP_ENABLE_TIME=1
    LP_USER_ALWAYS=1
    zplug "nojhan/liquidprompt"

    # Navigate to the .git project root
    zplug "mollifier/cd-gitroot"
    alias cdu='cd-gitroot'

    zplug load
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
    source ~/.fzf.zsh
fi
export FZF_TMUX=1

# Node - Node.js
PATH="$PATH:.node/bin"

# Go - The go language
export GOPATH=~/go
export PATH="$PATH:$GOPATH/bin"
export PATH="/usr/local/bin:$PATH"
export PATH="$PATH:/usr/local/opt/go/libexec/bin"

# Nvm - Node version manager
export NVM_DIR=~/.nvm
if type brew > /dev/null; then
    source $(brew --prefix nvm)/nvm.sh
    nvm use 0.12 &> /dev/null
else
    if [[ -f ~/.nvm/nvm.sh ]]; then
        source ~/.nvm/nvm.sh
        nvm use 0.12 &> /dev/null
    fi
fi

# Fuck - command correction
if type thefuck > /dev/null; then
    eval $(thefuck --alias)
fi

# Explicitely set language
export LC_CTYPE=en_US.UTF-8
export LC_ALL=en_US.UTF-8
