## Toolbag

### About

This repo contains all of the common config files used across my UNIX-based
systems. The idea is that you pull down the repo on a new machine, run the
(newly crafted) Python script, and you will then have the original files
replaced with symlinks that point into this repo. This provides an easy way to
have a consistent and syncronized working environment on any machine you're
using.

### Ctags

In order to use any tags functionality, you will want to install exuberant-ctags.

#### OSX

```bash
$ OLD_CTAGS=`which ctags`
$ brew install ctags
$ NEW_CTAGS=`which ctags`  # This will be the ctags that was installed by brew.
$ sudo rm $OLD_CTAGS
$ sudo ln -s $NEW_CTAGS $OLD_CTAGS
```

#### Ubuntu

```bash
$ sudo apt-get install exuberant-ctags
$ OLD_CTAGS=`which ctags`
$ sudo rm $OLD_CTAGS
$ sudo ln -s `which ctags-exuberant` $OLD_CTAGS
```
### Syntax checking

In order for Syntastic to be able to check for syntax errors on write, you
will need to have certain external analysis tools installed. For example,
syntax checking for Python requires that you install Pylint:

```bash
$ pip install pylint
```

### Updates

I switched from a bash script to a Python script for managing and symlinking the
files. This was much cleaner, and allows for the list of files to manage to be
updated easily.

### Installation

You only need to perform these steps once. After performing an installation,
updates to the symlinked files will be updates to the repo, which you can then
commit and push.

1. Clone the repo:

```bash
git clone git@github.com:wilkystyle/toolbag.git
```

1. Navigate into the repo directory:

```bash
cd toolbag
```

1. Run the installation Python script to create symlinks:

```bash
./make_links.py
```


### Staying up to date

In order to make sure you have the latest version of the files, simply do:

```bash
git pull
```
