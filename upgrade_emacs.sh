#!/bin/bash

command -v emacs >/dev/null 2>&1 || {
    echo "Emacs is not even installed! Please refer to the official Emacs documentation for installation instructions instead! Aborting..."
    exit 1

}

read -p "About to upgrade Emacs! This will install emacs-snapshot, move the existing emacs binary to emacs-old, and then symlink emacs-snapshot as emacs. You will need sudo permissions. Press ENTER to continue, or CTRL-C to abort..."

sudo apt-get update

command -v apt-add-repository >/dev/null 2>&1 || {
    read -p "apt-add-repository is required, but not found! Press ENTER to install it, or CTRL-C to abort..."
    sudo apt-get install -y python-software-properties
}

sudo add-apt-repository ppa:cassou/emacs
sudo apt-get update

sudo apt-get install -y emacs-snapshot

EMACS_DIR=$(dirname `which emacs`)
EMACS_BACKUP_NAME="emacs-old"
OLD_EMACS=$(which emacs)
NEW_EMACS=$(which emacs-snapshot)

sudo mv $OLD_EMACS $EMACS_DIR/$EMACS_BACKUP_NAME
sudo ln -s $NEW_EMACS $OLD_EMACS

echo "Done!"
emacs --version
