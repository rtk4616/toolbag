# OS-specific variables
if [[ `uname` == 'Linux' ]]; then
   COLOR_OPTION_STRING='--color=auto'
elif [[ `uname` == 'Darwin' ]]; then
   COLOR_OPTION_STRING='-G'
   alias flush_dns='sudo dscacheutil -flushcache && sudo killall -HUP mDNSResponder'
fi

# Paths.
export PATH=/usr/local/bin:/opt/local/lib/postgresql91/bin:/usr/local/share/npm/bin:/usr/local/sbin:$PATH
export WORKON_HOME=$HOME/.virtualenvs
export PROJECT_HOME=$HOME/Devel
export PYTHONSTARTUP=~/.pythonrc
# Golang bin path.
export GOPATH=$HOME/go
export PATH=$PATH:$GOPATH/bin
eval "$(rbenv init -)"

# Stuff needed for fssh function.
complete -F _known_hosts fssh
export PATH=~/toolbag/scripts:$PATH

# Misc.
export TERM=screen-256color
export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8
export LANGUAGE=en_US.UTF-8
export EDITOR="vi"

# Aliases.
alias pt='ptpython'
alias emacs='emacs -nw'
alias vims='vim -S ~/.session'
alias emux='tmux set -gw mode-keys emacs && tmux set -gw status-keys emacs'
alias l="ls -ph $COLOR_OPTION_STRING"
alias la="ls -al -ph $COLOR_OPTION_STRING"
alias ll="ls -plh $COLOR_OPTION_STRING"
alias tree='tree -C'
alias usage='sudo du -h --max-depth=1 | sort -h'
alias vmux='tmux set -gw mode-keys vi && tmux set -gw status-keys vi'

# Only execute if we have an interactive shell.
if ! [ -z "$PS1" ] && ! [ "$TERM" == "dumb" ]; then
    # Disable xon/xoff, which interferes with forward isearch bash history with C-s.
    stty -ixon

    # Source the virtualenvwrapper script.
    source /usr/local/bin/virtualenvwrapper.sh

    # --- colors ---
    BLACK='\[\e[0;30m\]'
    BLUE='\[\e[0;34m\]'
    GREEN='\[\e[0;32m\]'
    CYAN='\[\e[0;36m\]'
    RED='\[\e[0;31m\]'
    PURPLE='\[\e[0;35m\]'
    BROWN='\[\e[0;33m\]'
    LIGHTGRAY='\[\e[0;37m\]'
    DARKGRAY='\[\e[1;30m\]'
    LIGHTBLUE='\[\e[1;34m\]'
    LIGHTGREEN='\[\e[1;32m\]'
    LIGHTCYAN='\[\e[1;36m\]'
    LIGHTRED='\[\e[1;31m\]'
    LIGHTPURPLE='\[\e[1;35m\]'
    ORANGE='\[\e[1;33m\]'
    WHITE='\[\e[1;37m\]'
    NC='\[\e[0m\]'
    MAINCOLOR="$LIGHTGREEN"

    # --- set the prompt ---
    PS1="$MAINCOLOR[ $LIGHTBLUE\u$MAINCOLOR @ $LIGHTPURPLE\h$MAINCOLOR ]\n$MAINCOLOR\$PWD$NC $ "

    # Better backwards kill word with ctrl-w.
    stty werase undef
    bind '"\C-w": backward-kill-word'

    ### Added by the Heroku Toolbelt
    export PATH="/usr/local/heroku/bin:$PATH"
fi

function pull_commit_from_repo {
    USAGE_MESSAGE="\nAbout:\n
    Execute this command from within the Git repo you wish to pull the patch into.\n
    \n
    Usage:\n
    pull_commit_from_repo [COMMIT HASH] [FULL PATH TO .git FOLDER]\n"

    if [ -z "$1" ]
    then
        echo -e $USAGE_MESSAGE
        return
    fi

    if [ -z "$2" ]
    then
        echo -e $USAGE_MESSAGE
        return
    fi

    echo "About to pull $1 from the Git repo at $2"
    read -p "Press ENTER to confirm, or CTRL-C to abort..."
    git --git-dir="$2" format-patch -k -1 --stdout "$1" | git am -3 -k
}
