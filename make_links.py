#!/usr/bin/env python

import os

home_dir = os.path.expanduser("~")
repo_root = os.path.realpath(os.path.dirname(__file__))
backup_files_dir = os.path.join(home_dir, 'toolbag_file_backup')

print
print "home_dir is {0}".format(home_dir)
print "repo_root is {0}".format(repo_root)
print "backup_files_dir is {0}".format(backup_files_dir)
print

# Create a directory for backing up existing non-symlinked files.
if not os.path.exists(backup_files_dir):
    print
    print "Making a directory to store backed-up files in: {0}".format(
        backup_files_dir
    )
    print
    os.makedirs(backup_files_dir)

# These are the files that are to be symlinked.
managed_files = [
    ".agignore",
    ".ctags",
    ".elisp",
    ".emacs",
    ".emacs.d",
    ".eslintrc",
    ".gitconfig",
    ".ideavimrc",
    ".pylintrc",
    ".pythonrc",
    ".tmux.conf",
    ".vim",
    ".vimrc",
]

for the_file in managed_files:
    # The actual location of the file.
    actual_file_path = os.path.join(home_dir, the_file)

    if os.path.islink(actual_file_path):
        print "{0} is already symlinked! Skipping...".format(the_file)
    else:
        move_path = os.path.join(backup_files_dir, the_file)
        repo_path = os.path.join(repo_root, the_file)
        print "Moving from {0} to {1}".format(actual_file_path, move_path)

        # Move the file to the backup directory.
        try:
            os.rename(actual_file_path, move_path)
        except OSError:
            # No existing file to move. Don't worry about it!
            pass

        # Then create a symlink back where we just moved it from.
        print "Creating a symlink at {0} to {1}".format(
            actual_file_path,
            repo_path
        )
        os.symlink(repo_path, actual_file_path)
