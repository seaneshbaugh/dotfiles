#!/bin/bash

function show_help {
    echo "Usage: ./install.sh [switches]"
    echo "  -b[backup_directory]    set the backup directory"
    echo "  -d                      dry run, do not touch the file system (also sets verbose flag)"
    echo "  -f[files]               set the files to install"
    echo "  -h                      show this message and exit"
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

function detect_platform {
    case "$(uname -s)" in
        Darwin)
            echo "darwin"
            ;;
        Linux)
            if [[ -r /etc/os-release ]]; then
                . /etc/os-release
                case "$ID" in
                    ubuntu)
                        echo "ubuntu"
                        ;;
                    fedora)
                        echo "fedora"
                        ;;
                    *)
                        # Some other version of Linux without specific configuration.
                        echo "linux"
                        ;;
                esac
            else
                echo "linux"
            fi
            ;;
        *)
            echo "unknown"
            ;;
    esac
}

function is_platform_replacement_dotfile {
    local dotfile="$1"

    for replacement in "${PLATFORM_REPLACEMENT_DOTFILES[@]}"; do
        [[ "$replacement" = "$dotfile" ]] && return 0
    done

    return 1
}

function install_dotfile {
    local dotfile="$1"
    local destination="${2:-$dotfile}"
    local source_path="$DOTFILE_DIRECTORY/$dotfile"
    local dotfile_path="$HOME/.$destination"

    if [[ ! -e "$source_path" ]]; then
        echo "Warning: $source_path does not exist, skipping." >&2
        return
    fi

    if [[ -e "$dotfile_path" || -L "$dotfile_path" ]]; then
        local dotfile_name="$(basename "$dotfile_path")"
        local backup_path="$BACKUP_DIRECTORY/$dotfile_name"

        if [[ "$VERBOSE" -eq 1 ]]; then
            echo "$dotfile_path already exists."
        fi

        if [[ -L "$dotfile_path" ]]; then
            local symlink_path="$(readlink "$dotfile_path")"

            if [[ "$VERBOSE" -eq 1 ]]; then
                echo "$dotfile_path is a symlink pointing to $symlink_path."
            fi

            if [[ -e "$dotfile_path" ]]; then
                if [[ "$VERBOSE" -eq 1 ]]; then
                    echo "$dotfile_path is a valid symlink."
                fi

                if [[ "$dotfile_path" -ef "$source_path" ]]; then
                    if [[ "$VERBOSE" -eq 1 ]]; then
                        echo "$dotfile_path is already pointing $source_path, skipping."
                    fi

                    return
                else
                    if [[ "$VERBOSE" -eq 1 ]]; then
                        echo "$dotfile_path is pointing to a file other than $source_path."
                    fi

                    local new_symlink_path="$(realpath "$dotfile_path" "--relative-to=$BACKUP_DIRECTORY")"

                    if [[ "$VERBOSE" -eq 1 ]]; then
                        echo "Backing up old symlink with corrected path $new_symlink_path."
                    fi

                    if [[ "$DRYRUN" -eq 0 ]]; then
                        ln -s "$new_symlink_path" "$backup_path"
                        rm "$dotfile_path"
                    fi
                fi
            else
                if [[ "$VERBOSE" -eq 1 ]]; then
                    echo "$dotfile_path is not a valid symlink."
                fi

                if [[ "$VERBOSE" -eq 1 ]]; then
                    echo "Backing up old symlink as is with no path correction."
                fi

                if [[ "$DRYRUN" -eq 0 ]]; then
                    ln -s "$symlink_path" "$backup_path"
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
            echo "Creating new symlink for $dotfile_path to $source_path."
        fi

        if [[ "$DRYRUN" -eq 0 ]]; then
            ln -s "$source_path" "$dotfile_path"
        fi
    else
        if [[ "$VERBOSE" -eq 1 ]]; then
            echo "$dotfile_path does not already exist, creating symlink to $source_path."
        fi

        if [[ "$DRYRUN" -eq 0 ]]; then
            ln -s "$source_path" "$dotfile_path"
        fi
    fi
}

PLATFORM="$(detect_platform)"
INSTALL_SCRIPT_PATH="$(abspath "$0")"
DOTFILE_DIRECTORY="$(dirname "$INSTALL_SCRIPT_PATH")"
BACKUP_DIRECTORY="${DOTFILE_DIRECTORY}-backup"
DEFAULT_DOTFILES=(
    aliases
    bash_profile
    bashrc
    emacs.d
    gemrc
    gitconfig
    gnus.el
    tool-versions
    vimrc
    zlogin
    zlogout
    zshenv
    zshrc
)
DOTFILES=()
DEFAULT_PLATFORM_REPLACEMENT_DOTFILES=(
    tool-versions
)
PLATFORM_REPLACEMENT_DOTFILES=()
DEFAULT_PLATFORM_LOCAL_DOTFILES=(
    aliases
    gitconfig
    zshrc
)
PLATFORM_LOCAL_DOTFILES=()
DRYRUN=0
VERBOSE=0

while getopts "b:df:hv" opt; do
    case "$opt" in
        b)
            BACKUP_DIRECTORY="$OPTARG"

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
            PLATFORM_LOCAL_DOTFILES+=("$OPTARG")
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
    DOTFILES=("${DEFAULT_DOTFILES[@]}")
fi

if [[ ${#PLATFORM_REPLACEMENT_DOTFILES[@]} -eq 0 ]]; then
    PLATFORM_REPLACEMENT_DOTFILES=("${DEFAULT_PLATFORM_REPLACEMENT_DOTFILES[@]}")
fi

if [[ ${#PLATFORM_LOCAL_DOTFILES[@]} -eq 0 ]]; then
    PLATFORM_LOCAL_DOTFILES=("${DEFAULT_PLATFORM_LOCAL_DOTFILES[@]}")
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

for dotfile in "${DOTFILES[@]}"; do
    platform_replacement_dotfile="$dotfile.$PLATFORM"

    if is_platform_replacement_dotfile "$dotfile" && [[ -e "$DOTFILE_DIRECTORY/$platform_replacement_dotfile" ]]; then
        if [[ "$VERBOSE" -eq 1 ]]; then
            echo "$platform_replacement_dotfile is a replacement dotfile for $PLATFORM."
        fi

        case "$PLATFORM" in
            darwin|fedora|ubuntu)
                if [[ "$VERBOSE" -eq 1 ]]; then
                    echo "$PLATFORM is a supported platform, installing replacement dotfile."
                fi

                install_dotfile "$platform_replacement_dotfile" "$dotfile"
                ;;
            *)
                if [[ "$VERBOSE" -eq 1 ]]; then
                    echo "$PLATFORM is not a supported platform, installing base dotfile."
                fi

                install_dotfile "$dotfile"
                ;;
        esac
    else
        install_dotfile "$dotfile"
    fi
done

case "$PLATFORM" in
    darwin|fedora|ubuntu)
        if [[ "$VERBOSE" -eq 1 ]]; then
            echo "$PLATFORM is a supported platform, installing local dotfiles."
        fi

        for dotfile in "${PLATFORM_LOCAL_DOTFILES[@]}"; do
            platform_local_dotfile="$dotfile.local.$PLATFORM"

            if [[ -e "$DOTFILE_DIRECTORY/$platform_local_dotfile" ]]; then
                if [[ "$VERBOSE" -eq 1 ]]; then
                    echo "$platform_local_dotfile is a local dotfile for $PLATFORM."
                fi

                install_dotfile "$platform_local_dotfile" "$dotfile.local"
            fi
        done
        ;;
esac
