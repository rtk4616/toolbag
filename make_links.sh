#!/bin/bash

HOMEPATH=$(eval echo ~${SUDO_USER})
REPONAME="toolbag"

VIFILE="$HOMEPATH/.vimrc"
VIFILEOLD="$HOMEPATH/.vimrc_OLD"
REPOVIFILE="$HOMEPATH/$REPONAME/.vimrc"

VIDIR="$HOMEPATH/.vim"
VIDIROLD="$HOMEPATH/.vim_OLD"
REPOVIDIR="$HOMEPATH/$REPONAME/.vim"

EMACSFILE="$HOMEPATH/.emacs"
EMACSFILEOLD="$HOMEPATH/.emacs_OLD"
REPOEMACSFILE="$HOMEPATH/$REPONAME/.emacs"

EMACSDIR="$HOMEPATH/.elisp"
EMACSDIROLD="$HOMEPATH/.elisp_OLD"
REPOEMACSDIR="$HOMEPATH/$REPONAME/.elisp"

TMUXFILE="$HOMEPATH/.tmux.conf"
TMUXFILEOLD="$HOMEPATH/.tmux.conf_OLD"
REPOTMUXFILE="$HOMEPATH/$REPONAME/.tmux.conf"

PYRCFILE="$HOMEPATH/.pythonrc"
PYRCFILEOLD="$HOMEPATH/.pythonrc_OLD"
REPOPYRCFILE="$HOMEPATH/$REPONAME/.pythonrc"

# Make the symbolic link for the .vimrc file.
if [ -L "$VIFILE" ]
then
    echo "$VIFILE is already symlinked! Delete the symlink and re-run this script if you wish to create new links! Skipping..."
elif [ -f "$VIFILE" ]
then
    echo "$VIFILE does exist. Copying to $VIFILEOLD"
    mv $VIFILE $VIFILEOLD
    echo "Creating symbolic link..."
    ln -s $REPOVIFILE $VIFILE
    echo "Done."
else
    echo "$VIFILE does not exist. Creating symbolic link..."
    ln -s $REPOVIFILE $VIFILE
    echo "Done."
fi

# Make the symbolic link for the .vim folder.
if [ -L "$VIDIR" ]
then
    echo "$VIDIR is already symlinked! Delete the symlink and re-run this script if you wish to create new links! Skipping..."
elif [ -d "$VIDIR" ]
then
    echo "$VIDIR does exist. Copying to $VIDIROLD"
    mv $VIDIR $VIDIROLD
    echo "Creating symbolic link..."
    ln -s $REPOVIDIR $VIDIR
    echo "Done."
else
    echo "$VIDIR does not exist. Creating symbolic link..."
    ln -s $REPOVIDIR $VIDIR
    echo "Done."
fi

# Make the symbolic link for the .tmux.conf file.
if [ -L "$TMUXFILE" ]
then
    echo "$TMUXFILE is already symlinked! Delete the symlink and re-run this script if you wish to create new links! Skipping..."
elif [ -f "$TMUXFILE" ]
then
    echo "$TMUXFILE does exist. Copying to $TMUXFILEOLD"
    mv $TMUXFILE $TMUXFILEOLD
    echo "Creating symbolic link..."
    ln -s $REPOTMUXFILE $TMUXFILE
    echo "Done."
else
    echo "$TMUXFILE does not exist. Creating symbolic link..."
    ln -s $REPOTMUXFILE $TMUXFILE
    echo "Done."
fi

# Make the symbolic link for the .pythonrc file.
if [ -L "$PYRCFILE" ]
then
    echo "$PYRCFILE is already symlinked! Delete the symlink and re-run this script if you wish to create new links! Skipping..."
elif [ -f "$PYRCFILE" ]
then
    echo "$PYRCFILE does exist. Copying to $PYRCFILEOLD"
    mv $PYRCFILE $PYRCFILEOLD
    echo "Creating symbolic link..."
    ln -s $REPOPYRCFILE $PYRCFILE
    echo "Done."
else
    echo "$PYRCFILE does not exist. Creating symbolic link..."
    ln -s $REPOPYRCFILE $PYRCFILE
    echo "Done."
fi

# Make the symbolic link for the .emacs file.
if [ -L "$EMACSFILE" ]
then
    echo "$EMACSFILE is already symlinked! Delete the symlink and re-run this script if you wish to create new links! Skipping..."
elif [ -f "$EMACSFILE" ]
then
    echo "$EMACSFILE does exist. Copying to $EMACSFILEOLD"
    mv $EMACSFILE $EMACSFILEOLD
    echo "Creating symbolic link..."
    ln -s $REPOEMACSFILE $EMACSFILE
    echo "Done."
else
    echo "$EMACSFILE does not exist. Creating symbolic link..."
    ln -s $REPOEMACSFILE $EMACSFILE
    echo "Done."
fi

# Make the symbolic link for the .elisp folder.
if [ -L "$EMACSDIR" ]
then
    echo "$EMACSDIR is already symlinked! Delete the symlink and re-run this script if you wish to create new links! Skipping..."
elif [ -d "$EMACSDIR" ]
then
    echo "$EMACSDIR does exist. Copying to $EMACSDIROLD"
    mv $EMACSDIR $EMACSDIROLD
    echo "Creating symbolic link..."
    ln -s $REPOEMACSDIR $EMACSDIR
    echo "Done."
else
    echo "$EMACSDIR does not exist. Creating symbolic link..."
    ln -s $REPOEMACSDIR $EMACSDIR
    echo "Done."
fi
