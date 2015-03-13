# String values that, when echoed, set the iTerm 2 tab color.
ITERM_RED="\033]6;1;bg;red;brightness;255\a \033]6;1;bg;green;brightness;000\a \033]6;1;bg;blue;brightness;000\a"
ITERM_ORANGE="\033]6;1;bg;red;brightness;255\a \033]6;1;bg;green;brightness;155\a \033]6;1;bg;blue;brightness;000\a"
ITERM_BLUE="\033]6;1;bg;red;brightness;155\a \033]6;1;bg;green;brightness;155\a \033]6;1;bg;blue;brightness;255\a"
ITERM_GREEN="\033]6;1;bg;red;brightness;155\a \033]6;1;bg;green;brightness;255\a \033]6;1;bg;blue;brightness;000\a"

# Change these variables in order to control how your iTerm 2 tab looks.
SESSION_NAME="Default tab title"
SESSION_TAB_COLOR="$ITERM_BLUE"

# OS-specific variables
if [[ `uname` == 'Linux' ]]; then
   COLOR_OPTION_STRING='--color=auto'
elif [[ `uname` == 'Darwin' ]]; then
   COLOR_OPTION_STRING='-G'
fi

# Aliases.
alias ll="ls -plh $COLOR_OPTION_STRING"
alias la="ls -al -ph $COLOR_OPTION_STRING"
alias l="ls -ph $COLOR_OPTION_STRING"
alias tree='tree -C'
alias em='emacs'
alias usage='sudo du -h --max-depth=1 | sort -h'
alias emux='tmux set -gw mode-keys emacs && tmux set -gw status-keys emacs'
alias vmux='tmux set -gw mode-keys vi && tmux set -gw status-keys vi'
alias fssh='ssh -R 52698:127.0.0.1:52698 '

# This is a workaround for the SublimeGit plugin, since it seems to have path
# issues in Yosemite. Note that this requires Sublime Text to be launched via
# the command line, in order for the environment variable to be picked up.
alias subl='PATH=/usr/local/Cellar/git/1.8.3.1/libexec/git-core/:$PATH subl'

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

VIRTUALENV_SCRIPT="/usr/local/bin/virtualenvwrapper.sh"

# Only execute if we have an interactive shell.
if ! [ -z "$PS1" ]; then
    # Don't change this! Edit the values of the following two variables, listed
    # near the top of this file!
    echo -e -n "\033];$SESSION_NAME\007"
    echo -e -n "$SESSION_TAB_COLOR"

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
