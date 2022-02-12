# dotfiles

My dotfiles to set up my shell environment. I mostly use macOS but these should generally work on Linux with a little modification.

You should probably make your own.

## Installation

    $ git clone git@github.com:seaneshbaugh/dotfiles.git
    $ cd dotfiles
    $ ./install.sh

The install script takes the following options:

    -b[backup_directory]    set the backup directory
    -d                      dry run, do not touch the file system (also sets verbose flag)
    -f[files]               set the files to install
    -h                      show help message
    -v                      show verbose output

## Notes

This is based off [Ryan Bates' old dotfile setup](https://github.com/ryanb/dotfiles/tree/custom-bash-zsh). [oh-my-zsh](https://github.com/robbyrussell/oh-my-zsh) is great, but is just a bit much. I like to keep my setup as close to the default as possible and change only what I know from personal experience the things I prefer.

## Useful Links

### Shell Resources

* http://blog.smalleycreative.com/tutorials/using-git-and-github-to-manage-your-dotfiles/
* http://zsh.sourceforge.net/Intro/intro_toc.html
* http://www.joshstaiger.org/archives/2005/07/bash_profile_vs.html
* http://www.softpanorama.info/Scripting/Shellorama/Control_structures/if_statements.shtml
* http://www.thegeekstuff.com/2008/09/bash-shell-ps1-10-examples-to-make-your-linux-prompt-like-angelina-jolie/
* http://www.grymoire.com/Unix/Quote.html
* http://www.acm.uiuc.edu/workshops/zsh/prompt/escapes.html
* http://www.shellcheck.net/

### Emacs Resources

* http://crypt.codemancers.com/posts/2013-09-26-setting-up-emacs-as-development-environment-on-osx
* http://swaroopch.com/2013/10/17/emacs-configuration-tutorial/
* http://cask.readthedocs.org/en/latest/index.html
* https://github.com/cask/cask
* https://github.com/rdallasgray/pallet
