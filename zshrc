SOURCE="${(%):-%N}"

while [ -h "$SOURCE" ]; do
    DIR="$( cd -P "$( dirname "$SOURCE" )" >/dev/null 2>&1 && pwd )"
    SOURCE="$(readlink "$SOURCE")"
    [[ $SOURCE != /* ]] && SOURCE="$DIR/$SOURCE"
done

DOTFILES_DIRECTORY="$( cd -P "$( dirname "$SOURCE" )" >/dev/null 2>&1 && pwd )"

. "$DOTFILES_DIRECTORY/zsh/config"
. "$DOTFILES_DIRECTORY/zsh/aliases"
[ -f "$HOME/.env" ] && . "$HOME/.env"

export PATH="${ASDF_DATA_DIR:=$HOME/.asdf}/shims:$PATH"
