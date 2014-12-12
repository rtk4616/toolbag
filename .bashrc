# Aliases.
alias ll='ls -plh'
alias la='ls -al -ph'
alias l='ls -ph'
alias tree='tree -C'
alias em='emacs'
alias usage='sudo du -h --max-depth=1 | sort -h'

# Paths.
export PATH=/usr/local/bin:/opt/local/lib/postgresql91/bin:/usr/local/share/npm/bin:/usr/local/sbin:$PATH
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

# Set the editor.
# NOTE: The .tmux.conf file assumes that you will be using one of:
#
# [emacs | vi]
#
# If this is not the case, you should edit the .tmux.conf file and
# explicitly specify the value.
export EDITOR="vi"

VIRTUALENV_SCRIPT="/usr/local/bin/virtualenvwrapper.sh"

# Only execute if we have an interactive shell.
if ! [ -z "$PS1" ]; then
    # iTerm2 stuff.
    echo -e -n "\033];Digital Ocean: wilkyhost\007"
    echo -e -n "\033]6;1;bg;red;brightness;120\a"
    echo -e -n "\033]6;1;bg;green;brightness;180\a"
    echo -e -n "\033]6;1;bg;blue;brightness;220\a"

    if [ -e "$VIRTUALENV_SCRIPT" ]
    then
        source $VIRTUALENV_SCRIPT
    fi

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
    YELLOW='\e[1;33m'
    WHITE='\e[1;37m'
    NC='\e[0m'              # No Color

    # Meta-colors
    BOLD='\e[1m'
    BLINK='\e[5m'
    REVERSE='\e[7m'

    # --- git branch ---
    git_branch() {
        if [ -e "$PWD/.git" ]; then
           git branch -v 2> /dev/null | grep '*' | awk '{if ($2) printf ("(%s) ", $2) }'
        fi
    }

    # --- ve warning ---
    check_ve() {
        if [ -f ./bin/activate ] || [ -f ../bin/activate ] || [ -f ./ve/bin/activate ] || [ -f ../ve/bin/activate ]; then
            if [[ $VIRTUAL_ENV == "" ]] || [[ $PWD != *$VIRTUAL_ENV* ]] ; then
                #echo "You need to activate this ve"
                trash="nothing"
            fi
        fi
    }

    # --- set the prompt ---
    PS1="|\[\033[1;34m\]\$(git_branch) \[\033[1;34m\]\u\[\033[0m\] @ \[\033[1;35m\]\h\n\[\033[1;36m\]\$PWD\[\033[0m\] $ "

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
