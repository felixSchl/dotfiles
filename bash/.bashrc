SSH_ENV=$HOME/.ssh/environment
export PATH=/c/Python33:$PATH

function start_agent {
     echo "Initialising new SSH agent..."
     /usr/bin/ssh-agent | sed 's/^echo/#echo/' > ${SSH_ENV}
     echo succeeded
     chmod 600 ${SSH_ENV}
     . ${SSH_ENV} > /dev/null
     /usr/bin/ssh-add;
}

# Source SSH settings, if applicable

if [ -f "${SSH_ENV}" ]; then
     . ${SSH_ENV} > /dev/null
     #ps ${SSH_AGENT_PID} doesn't work under cywgin
     ps -ef | grep ${SSH_AGENT_PID} | grep ssh-agent$ > /dev/null || {
         start_agent;
     }
else
     start_agent;
fi

# ------------------------------------------------------------ #
# ALIASES

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

# Set general aliases.
alias e='emacs -nw'
alias la='ls -a'
alias ll='ls -g'
alias t='tree'
alias k='tree -L 2'

alias ez='emacs ~/.zshrc'
alias ee='emacs ~/.emacs.d/init.el'

# Find largest files.
alias biggest='find -type f -printf '\''%s %p\n'\'' | sort -nr | head -n 40 | gawk "{ print \$1/1000000 \" \" \$2 \" \" \$3 \" \" \$4 \" \" \$5 \" \" \$6 \" \" \$7 \" \" \$8 \" \" \$9 }"'

# ------------------------------------------------------------ #
# GIT ALIASES

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

# ------------------------------------------------------------ #
# SHELL COLOURS

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

# ------------------------------------------------------------ #
# CUSTOMISE PROMPT

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
