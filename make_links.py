#!/usr/bin/env python

import os
import sys

home_dir = os.path.expanduser("~")
repo_root = os.path.realpath(os.path.dirname(__file__))
backup_files_dir = os.path.join(home_dir, 'toolbag_file_backup')

print
print "home_dir is {}".format(home_dir)
print "repo_root is {}".format(repo_root)
print "backup_files_dir is {}".format(backup_files_dir)
print

# Create a directory for backing up existing non-symlinked files.
if not os.path.exists(backup_files_dir):
    print
    print "Making a directory to store backed-up files in: {}".format(
        backup_files_dir
    )
    print
    os.makedirs(backup_files_dir)

# These are the files that are to be symlinked.
managed_files = [
    ".vimrc",
    ".vim",
    ".emacs",
    ".elisp",
    ".tmux.conf",
    ".pythonrc",
    ".gitconfig",
]

for the_file in managed_files:
    # The actual location of the file.
    actual_file_path = os.path.join(home_dir, the_file)

    if os.path.islink(actual_file_path):
        print "{} is already symlinked! Skipping...".format(the_file)
    else:
        move_path = os.path.join(backup_files_dir, the_file)
        repo_path = os.path.join(repo_root, the_file)
        print "Moving from {} to {}".format(actual_file_path, move_path)

        # Move the file to the backup directory.
        os.rename(actual_file_path, move_path)

        # Then create a symlink back where we just moved it from.
        print "Creating a symlink at {} to {}".format(
            actual_file_path,
            repo_path
        )
        os.symlink(repo_path, actual_file_path)