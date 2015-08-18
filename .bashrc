# OS-specific variables
if [[ `uname` == 'Linux' ]]; then
   COLOR_OPTION_STRING='--color=auto'
elif [[ `uname` == 'Darwin' ]]; then
   COLOR_OPTION_STRING='-G'
fi

# Paths.
export PATH=~/toolbag/scripts:/usr/local/bin:/opt/local/lib/postgresql91/bin:/usr/local/share/npm/bin:/usr/local/sbin:$PATH
export WORKON_HOME=$HOME/.virtualenvs
export PROJECT_HOME=$HOME/Devel
export PYTHONSTARTUP=~/.pythonrc
# Golang bin path.
export GOPATH=$HOME/go
export PATH=$PATH:$GOPATH/bin

# Misc.
export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8
export LANGUAGE=en_US.UTF-8
export EDITOR="vi"

# Aliases.
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
if ! [ -z "$PS1" ]; then
    # Disable xon/xoff, which interferes with forward isearch bash history with C-s.
    stty -ixon

    # --- colors ---
    BLACK='\e[0;30m'
    BLUE='\e[0;34m'
    GREEN='\e[0;32m'
    CYAN='\e[0;36m'
    RED='\e[0;31m'
    PURPLE='\e[0;35m'
    BROWN='\e[0;33m'
    LIGHTGRAY='\e[0;37m'
    DARKGRAY='\e[1;30m'
    LIGHTBLUE='\e[1;34m'
    LIGHTGREEN='\e[1;32m'
    LIGHTCYAN='\e[1;36m'
    LIGHTRED='\e[1;31m'
    LIGHTPURPLE='\e[1;35m'
    ORANGE='\e[1;33m'
    WHITE='\e[1;37m'
    NC='\e[0m'
    MAINCOLOR="$LIGHTCYAN"

    # Meta-colors
    BOLD='\e[1m'
    BLINK='\e[5m'
    REVERSE='\e[7m'

    # --- set the prompt ---
    PS1="$MAINCOLOR[ $LIGHTBLUE\u$MAINCOLOR @ $LIGHTPURPLE\h$MAINCOLOR ]\n$MAINCOLOR\$PWD$NC $ "

    # Better backwards kill word with ctrl-w.
    stty werase undef
    bind '"\C-w": backward-kill-word'

    ### Added by the Heroku Toolbelt
    export PATH="/usr/local/heroku/bin:$PATH"
fi

function fssh {
    # SSH function for using rmate on remote servers to open files in a local
    # instance of Sublime Text with the rsub plugin. This function will ensure
    # that an up-to-date copy of the rmate script from my git repo exists in
    # /tmp/, unless there already exists a file/directory by the same name in
    # /tmp/ that is NOT some version of my rmate script.rmate
    #
    # This function looks for a simple hash in a comment at the end of the
    # rmate script to determine whether or not /tmp/rmate is my script.
    #
    # This function wraps the ssh command with the following:
    #
    # - Flags for creating a reverse tunnel on port 52698.
    # - All user-supplied arguments.
    # - The -t flag, to force establishing a tty.
    # - A series of bash commands to run on the remote host that will wget the
    #   latest version of my rmate script from my GitHub repo, unless there is
    #   an existing file or directory that is NOT my rmate script.
    #
    # (That last step is so that I don't overwrite someone else's file, on the
    # off-chance that a file with different contents exists at the same
    # name/location)
    if [ $# -eq 0 ]; then
        echo "No arguments provided"
        return
    fi
    ssh -R 52698:127.0.0.1:52698 "$@" -t \
    '\
    echo; \
    echo "-------------------------------------"; \
    echo "SSH with reverse tunnel on port 52698"; \
    echo "-------------------------------------"; \
    ([ ! -e /tmp/rmate ] || [ "# rmate id 8355c6053581459eaee307e2b6e417b2" == "$(tail -1 /tmp/rmate)" ]) \
    && echo "Obtaining latest version of rmate script..." \
    && wget -q https://raw.githubusercontent.com/wilkystyle/toolbag/master/scripts/rmate -O /tmp/rmate \
    && chmod +x /tmp/rmate \
    && echo "Done."; \
    $SHELL \
    '
}

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
