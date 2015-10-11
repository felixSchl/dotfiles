# Antigen bundles
if [[ -f ~/antigen.zsh ]]; then
    source ~/antigen.zsh
    antigen use oh-my-zsh
    antigen bundle git                  # Completions for git
    antigen bundle node                 # Completions for node
    antigen bundle tmux                 # Completions for tmux
    antigen bundle python               # Completions for python
    antigen bundle brew                 # Completions for brew
    antigen bundle bower                # Completions for bower
    antigen bundle vi-mode              # Vim navigation for zsh
    antigen bundle hchbaw/opp.zsh       # Vim text objects for zsh
    antigen bundle nojhan/liquidprompt  # Prompt style
    antigen bundle mollifier/cd-gitroot # CD to nearest git repo
    antigen bundle zsh-users/zsh-syntax-highlighting
    antigen bundle zsh-users/zsh-history-substring-search
    antigen bundle tarruda/zsh-autosuggestions

    # Liquid prompt
    LP_ENABLE_TIME=1
    LP_USER_ALWAYS=1

    # Search history (in vi-mode)
    bindkey -M vicmd 'k' history-substring-search-up
    bindkey -M vicmd 'j' history-substring-search-down

    antigen apply
fi

# Exports
export EDITOR='vim'
export KEYTIMEOUT=1
export DISABLE_AUTO_TITLE=true

# Aliases
alias _='sudo'
alias tmux='tmux -2'
alias emacs='TERM=xterm-256color emacs -nw'
alias cdu='cd-gitroot'

# Fuzzy finder
if [[ -f ~/.fzf.zsh ]]; then
    source ~/.fzf.zsh
fi
