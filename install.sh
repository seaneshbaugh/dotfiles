#!/bin/bash

directory=~/dotfiles

backup_directory=~/dotfiles_backup

files="bash_profile bashrc emacs gemrc gitconfig vimrc zlogin zlogout zshenv zshrc"

mkdir -p $backup_directory

cd $directory

for file in $files; do
    mv ~/.$file $backup_directory

    ln -s $directory/$file ~/.$file
done
