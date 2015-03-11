source ~/antigen.zsh

antigen use oh-my-zsh

antigen bundle git
antigen bundle github
antigen bundle node
antigen bundle tmuxinator
antigen bundle tmux
antigen bundle python
antigen bundle command-not-found
antigen bundle brew
antigen bundle cabal
antigen bundle bower
antigen bundle vi-mode
antigen bundle hchbaw/opp.zsh
antigen bundle zsh-users/zsh-syntax-highlighting
antigen bundle zsh-users/zsh-history-substring-search
antigen bundle nojhan/liquidprompt
antigen bundle mollifier/cd-gitroot
antigen bundle jocelynmallon/zshmarks
antigen bundle tarruda/zsh-autosuggestions

# Liquid prompt
LP_ENABLE_TIME=1
LP_USER_ALWAYS=1

# Search history (in vi-mode)
bindkey -M vicmd 'k' history-substring-search-up
bindkey -M vicmd 'j' history-substring-search-down

# Conclude antigen
antigen apply

# Exports
export PATH="/usr/local/Cellar/fzf/0.9.2/bin:/usr/local/bin:/Users/felix/.node/bin:.cabal-sandbox/bin:/Users/felix/Library/Haskell/bin:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:/Users/felix/.rvm/bin:/Users/felix/.cabal/bin:/Users/felix/.node/bin"
export EDITOR='vim'
export KEYTIMEOUT=1
export DISABLE_AUTO_TITLE=true

# Aliases
alias cdu='cd-gitroot'
alias tmux='tmux -2'
alias tmuxinator='TERM=xterm-256color tmuxinator'
alias mux='TERM=xterm-256color mux'

# Fuzzy finder
source ~/.fzf.zsh
