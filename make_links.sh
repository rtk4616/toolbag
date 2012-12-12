#!/bin/bash

HOMEPATH=$(eval echo ~${SUDO_USER})
REPONAME="vi-config"
VIFILE="$HOMEPATH/.vimrc"
VIFILEOLD="$HOMEPATH/.vimrc_OLD"
REPOVIFILE="$HOMEPATH/$REPONAME/.vimrc"
VIDIR="$HOMEPATH/.vim"
VIDIROLD="$HOMEPATH/.vim_OLD"
REPOVIDIR="$HOMEPATH/$REPONAME/.vim"

# Make the symbolic link for .vimrc file.
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
