SOURCE="${(%):-%N}"

while [ -h "$SOURCE" ]; do
    DIR="$( cd -P "$( dirname "$SOURCE" )" >/dev/null 2>&1 && pwd )"
    SOURCE="$(readlink "$SOURCE")"
    [[ $SOURCE != /* ]] && SOURCE="$DIR/$SOURCE"
done

DOTFILES_DIRECTORY="$( cd -P "$( dirname "$SOURCE" )" >/dev/null 2>&1 && pwd )"

[[ -f "$HOME/.env" ]] && . "$HOME/.env"
. "$DOTFILES_DIRECTORY/common/config"
. "$DOTFILES_DIRECTORY/zsh/config"
. "$DOTFILES_DIRECTORY/zsh/aliases"

[[ -f "$HOME/.local/bin/env" ]] && . "$HOME/.local/bin/env"

# TODO: Remove this once all machines are migrated from asdf to mise.
[[ -d "${ASDF_DATA_DIR:=$HOME/.asdf}/shims" ]] && export PATH="${ASDF_DATA_DIR:=$HOME/.asdf}/shims:$PATH"
