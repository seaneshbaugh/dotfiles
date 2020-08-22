SOURCE="${(%):-%N}"

while [ -h "$SOURCE" ]; do
    DIR="$( cd -P "$( dirname "$SOURCE" )" && pwd )"
    SOURCE="$(readlink "$SOURCE")"
    [[ $SOURCE != /* ]] && SOURCE="$DIR/$SOURCE"
done

DOTFILES_DIRECTORY="$( cd -P "$( dirname "$SOURCE" )" && pwd )"

. "$DOTFILES_DIRECTORY/zsh/config"
. "$DOTFILES_DIRECTORY/zsh/aliases"
[ -f "$DOTFILES_DIRECTORY/secrets" ] && . "$DOTFILES_DIRECTORY/secrets"
[ -f "/usr/local/opt/asdf/asdf.sh" ] && . "/usr/local/opt/asdf/asdf.sh"

#. ~/.teladoc.sh

#zstyle ':completion:*:*:git:*' script ./git/git-completion.zsh
