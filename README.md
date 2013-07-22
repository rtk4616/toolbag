## Toolbag

### About

This repo contains all of the common config files used across my UNIX-based
systems. The idea is that you pull down the repo on a new machine, run the
(newly crafted) Python script, and you will then have the original files
replaced with symlinks that point into this repo. This provides an easy way to
have a consistent and syncronized working environment on any machine you're
using.

### Updates

I switched from a bash script to a Python script for managing and symlinking the
files. This was much cleaner, and allows for the list of files to manage to be
updated easily.

### Installation

You only need to perform these steps once. After performing an installation,
updates to the symlinked files will be updates to the repo, which you can then
commit and push.

1. Clone the repo:

    `git clone git@github.com:wilkystyle/toolbag.git`

1. Navigate into the repo directory:

    `cd toolbag`

1. Run the installation Python script to create symlinks:

    `./make_links.py`


### Staying up to date

In order to make sure you have the latest version of the files, simply do:

> `git pull`