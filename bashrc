SOURCE="${BASH_SOURCE[0]}"

while [ -h "$SOURCE" ]; do
    DIR="$( cd -P "$( dirname "$SOURCE" )" >/dev/null 2>&1 && pwd )"
    SOURCE="$(readlink "$SOURCE")"
    [[ $SOURCE != /* ]] && SOURCE="$DIR/$SOURCE"
done

DOTFILES_DIRECTORY="$( cd -P "$( dirname "$SOURCE" )" >/dev/null 2>&1 && pwd )"

. "$DOTFILES_DIRECTORY/bash/config"
. "$DOTFILES_DIRECTORY/bash/aliases"
[ -f "$DOTFILES_DIRECTORY/secrets" ] && . "$DOTFILES_DIRECTORY/secrets"
[ -f "/usr/local/opt/asdf/asdf.sh" ] && . "/usr/local/opt/asdf/asdf.sh"
