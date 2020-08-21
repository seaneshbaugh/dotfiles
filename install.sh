#!/bin/bash

function show_help {
    echo "Usage: ./install.sh [switches]"
    echo "  -b[backup_directory]    set the backup directory"
    echo "  -d                      dry run, do not touch the file system (also sets verbose flag)"
    echo "  -f[files]               set the files to install"
    echo "  -h                      show this message"
    echo "  -v                      show verbose output"
}

function abspath {
    if [[ -d "$1" ]]
    then
	pushd "$1" >/dev/null || exit 1
        pwd
        popd >/dev/null || exit 1
    elif [[ -e "$1" ]]
    then
        pushd "$(dirname "$1")" >/dev/null || exit 1
        echo "$(pwd)/$(basename "$1")"
        popd >/dev/null || exit 1
    else
	echo "$1 does not exist!" >&2
	return 127
    fi
}

function expand_tilde {
    case "$1" in
        (\~)        echo "$HOME";;
        (\~/*)      echo "$HOME/${1#\~/}";;
        (\~[^/]*/*) local user
                    user="$(eval echo "${1%%/*}")"
                    echo "$user/${1#*/}";;
        (\~[^/]*)   eval echo ${1};;
        (*)         echo "$1";;
    esac
}

INSTALL_SCRIPT_PATH="$(abspath "$0")"
DOTFILE_DIRECTORY="$(dirname "$INSTALL_SCRIPT_PATH")"
BACKUP_DIRECTORY="${DOTFILE_DIRECTORY}_backup"
DEFAULT_DOTFILES="bash_profile bashrc emacs.d gemrc gitconfig gnus.el tool-versions vimrc zlogin zlogout zshenv zshrc"
DOTFILES=()
DRYRUN=0
VERBOSE=0

while getopts "b:df:hv" opt; do
    case "$opt" in
        b)
            BACKUP_DIRECTORY="$(expand_tilde "$OPTARG")"

            if [[ "$BACKUP_DIRECTORY" = "$HOME" || "$BACKUP_DIRECTORY" = "$DOTFILE_DIRECTORY" ]]; then
                echo "Error: Cannot use home directory or $DOTFILE_DIRECTORY as backup directory." >&2
                exit 1
            fi
            ;;
        d)
            DRYRUN=1
            VERBOSE=1
            ;;
        f)
            DOTFILES+=("$OPTARG")
            ;;
        h)
            show_help
            exit 0
            ;;
        v)
            VERBOSE=1
            ;;
        *)
            echo "Error: Invalid switch." >&2
            show_help
            exit 1
            ;;
    esac
done

if [[ ${#DOTFILES[@]} -eq 0 ]]; then
    for dotfile in $DEFAULT_DOTFILES; do
        DOTFILES+=("$dotfile")
    done
fi

if [[ "$DRYRUN" -eq 1 ]]; then
    echo "Doing dry run. No files will be modified."
fi

if [[ "$VERBOSE" -eq 1 ]]; then
    echo "Installing dotfiles from $DOTFILE_DIRECTORY."
fi

if [[ "$VERBOSE" -eq 1 ]]; then
    echo "Creating backup directory $BACKUP_DIRECTORY."
fi

if [[ "$DRYRUN" -eq 0 ]]; then
    mkdir -p "$BACKUP_DIRECTORY"
fi

for dotfile in ${DOTFILES[@]}; do
    source_path="$DOTFILE_DIRECTORY/$dotfile"
    dotfile_path="$HOME/.$dotfile"

    if [[ -e $dotfile_path ]]; then
        dotfile_name="$(basename "$dotfile_path")"
        backup_path="$BACKUP_DIRECTORY/$dotfile_name"

        if [[ "$VERBOSE" -eq 1 ]]; then
            echo "$dotfile_path already exists."
        fi

        if [[ -L $dotfile_path ]]; then
            symlink_path="$(readlink "$dotfile_path")"

            if [[ "$VERBOSE" -eq 1 ]]; then
                echo "$dotfile_path is a symlink pointing to $symlink_path."
            fi

            symlink_directory="$(dirname "$symlink_path")"

            if [[ "$symlink_directory" = "$DOTFILE_DIRECTORY" ]]; then
                if [[ "$VERBOSE" -eq 1 ]]; then
                    echo "$dotfile_path is pointing to a file in $DOTFILE_DIRECTORY, skipping."
                fi

                continue
            else
                if [[ "$VERBOSE" -eq 1 ]]; then
                    echo "$dotfile_path is pointing to a file outside $DOTFILE_DIRECTORY."
                fi

                if [[ "$VERBOSE" -eq 1 ]]; then
                    echo "Backing up old symlink."
                fi

                new_symlink_path="$(realpath "$dotfile_path" "--relative-to=$BACKUP_DIRECTORY")"

                if [[ "$DRYRUN" -eq 0 ]]; then
                    ln -s "$new_symlink_path" "$backup_path"
                    rm "$dotfile_path"
                fi
            fi
        else
            if [[ "$VERBOSE" -eq 1 ]]; then
                echo "$dotfile_path is a real file or directory."
            fi

            if [[ "$VERBOSE" -eq 1 ]]; then
                echo "Backing up old file or directory."
            fi

            if [[ "$DRYRUN" -eq 0 ]]; then
                mv "$dotfile_path" "$backup_path"
            fi
        fi

        if [[ "$VERBOSE" -eq 1 ]]; then
            echo "Creating new symlink for $dotfile_path."
        fi

        if [[ "$DRYRUN" -eq 0 ]]; then
            ln -s "$source_path" "$dotfile_path"
        fi
    else
        if [[ "$VERBOSE" -eq 1 ]]; then
            echo "$dotfile_path does not already exist, creating symlink."
        fi

        if [[ "$DRYRUN" -eq 0 ]]; then
            ln -s "$source_path" "$dotfile_path"
        fi
    fi
done
