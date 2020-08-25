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

## Emacs Config Stuff

### Setup

Install Cask

    $ brew install cask

After installing the .emacs.d symlink

    $ cd ~/.emacs.d
    $ cask install

### ELPA Sources

The following ELPA sources are used:

* [gnu](http://elpa.gnu.org/packages/)
* [melpa](http://melpa.org/#/)
* [org](http://orgmode.org/elpa/)

### ELPA Packages

The following ELPA packages are used:

* auto-complete: [melpa](https://melpa.org/#/auto-complete),  [github](https://github.com/auto-complete/auto-complete)
* bind-key: [melpa](https://melpa.org/#/bind-key), [github](https://github.com/jwiegley/use-package)
* cask: [melpa](https://melpa.org/#/cask), [github](https://github.com/cask/cask)
* clojure-mode: [melpa](https://melpa.org/#/clojure-mode), [github](https://github.com/clojure-emacs/clojure-mode)
* coffee-mode: [melpa](https://melpa.org/#/coffee-mode), [github](https://github.com/defunkt/coffee-mode)
* csharp-mode: [melpa](https://melpa.org/#/csharp-mode), [github](https://github.com/josteink/csharp-mode)
* d-mode: [melpa](https://melpa.org/#/d-mode), [github](https://github.com/Emacs-D-Mode-Maintainers/Emacs-D-Mode)
* dash: [melpa](https://melpa.org/#/dash), [github](https://github.com/magnars/dash.el)
* drag-stuff: [melpa](https://melpa.org/#/drag-stuff), [github](https://github.com/rejeep/drag-stuff.el)
* elixir-mode: [melpa](https://melpa.org/#/elixir-mode), [github](https://github.com/elixir-lang/emacs-elixir)
* enh-ruby-mode: [melpa](https://melpa.org/#/enh-ruby-mode), [github](https://github.com/zenspider/enhanced-ruby-mode)
* erlang: [melpa](https://melpa.org/#/erlang), [source](http://www.erlang.org/download/contrib/erlang.el)
* ess: [melpa](https://melpa.org/#/ess), [github](https://github.com/emacs-ess/ESS)
* exec-path-from-shell: [melpa](https://melpa.org/#/exec-path-from-shell), [github](https://github.com/purcell/exec-path-from-shell)
* expand-region: [melpa](https://melpa.org/#/expand-region), [github](https://github.com/magnars/expand-region.el)
* f: [melpa](https://melpa.org/#/f), [github](https://github.com/rejeep/f.el)
* flycheck: [melpa](https://melpa.org/#/flycheck), [github](https://github.com/flycheck/flycheck)
* flycheck-cask: [melpa](https://melpa.org/#/flycheck-cask), [github](https://github.com/flycheck/flycheck-cask)
* fsharp-mode: [melpa](https://melpa.org/#/fsharp-mode), [github](https://github.com/rneatherway/emacs-fsharp-mode-bin)
* git-gutter: [melpa](https://melpa.org/#/git-gutter), [github](https://github.com/syohex/emacs-git-gutter)
* go-mode: [melpa](https://melpa.org/#/go-mode), [github](https://github.com/dominikh/go-mode.el)
* groovy-mode: [melpa](https://melpa.org/#/groovy-mode), [github](https://github.com/Groovy-Emacs-Modes/groovy-emacs-modes)
* haml-mode: [melpa](https://melpa.org/#/haml-mode),[github](https://github.com/nex3/haml-mode)
* haskell-mode: [melpa](https://melpa.org/#/haskell-mode), [github](https://github.com/haskell/haskell-mode)
* htmlize: [melpa](https://melpa.org/#/htmlize), [source](http://fly.srk.fer.hr/~hniksic/emacs/htmlize.el.cgi)
* idle-highlight-mode: [melpa](https://melpa.org/#/idle-highlight-mode), [github](https://github.com/nonsequitur/idle-highlight-mode)
* io-mode: [melpa](https://melpa.org/#/io-mode), [github](https://github.com/superbobry/io-mode)
* less-css-mode: [melpa](https://melpa.org/#/less-css-mode), [github](https://github.com/purcell/less-css-mode)
* lua-mode: [melpa](https://melpa.org/#/lua-mode), [github](https://github.com/immerrr/lua-mode)
* magit: [melpa](https://melpa.org/#/magit), [github](https://github.com/magit/magit/tree/master)
* markdown-mode: [melpa](https://melpa.org/#/markdown-mode), [github](https://github.com/defunkt/markdown-mode)
* multiple-cursors: [melpa](https://melpa.org/#/multiple-cursors), [github](https://github.com/magnars/multiple-cursors.el)
* neotree: [melpa](https://melpa.org/#/neotree), [github](https://github.com/jaypei/emacs-neotree)
* nginx-mode: [melpa](https://melpa.org/#/nginx-mode), [github](https://github.com/ajc/nginx-mode)
* nim-mode: [melpa](https://melpa.org/#/nim-mode), [github](https://github.com/reactormonk/nim-mode)
* no-easy-keys: [github](https://github.com/danamlund/emacs-no-easy-keys)
* nyan-mode: [melpa](https://melpa.org/#/nyan-mode), [github](https://github.com/TeMPOraL/nyan-mode)
* org-plus-contrib: [org](http://orgmode.org/elpa/), [source](http://orgmode.org/cgit.cgi/org-mode.git/)
* pallet: [melpa](https://melpa.org/#/pallet), [github](https://github.com/rdallasgray/pallet)
* pbcopy: [melpa](https://melpa.org/#/pbcopy), [github](https://github.com/emacsfodder/pbcopy.el)
* popwin: [melpa](https://melpa.org/#/popwin), [github](https://github.com/m2ym/popwin-el)
* prodigy: [melpa](https://melpa.org/#/prodigy), [github](https://github.com/rejeep/prodigy.el)
* projectile: [melpa](https://melpa.org/#/projectile), [github](https://github.com/bbatsov/projectile)
* rjsx-mode: [melpa](https://melpa.org/#/rjsx-mode), [github](https://github.com/felipeochoa/rjsx-mode)
* rubocop: [melpa](https://melpa.org/#/rubocop), [github](https://github.com/rubocop-hq/rubocop-emacs)
* rust-mode: [melpa](https://melpa.org/#/rust-mode),[github](https://github.com/rust-lang/rust-mode)
* s: [melpa](https://melpa.org/#/s),   [github](https://github.com/magnars/s.el)
* sass-mode: [melpa](https://melpa.org/#/sass-mode), [github](https://github.com/nex3/sass-mode)
* scala-mode: [melpa](https://melpa.org/#/scala-mode), [github](https://github.com/ensime/emacs-scala-mode)
* scss-mode: [melpa](https://melpa.org/#/scss-mode), [github](https://github.com/antonj/scss-mode)
* slim-mode: [melpa](https://melpa.org/#/slim-mode), [github](https://github.com/slim-template/emacs-slim)
* slime: [melpa](https://melpa.org/#/slime), [github](https://github.com/slime/slime)
* smartparens: [melpa](https://melpa.org/#/smartparens), [github](https://github.com/Fuco1/smartparens)
* smex: [melpa](https://melpa.org/#/smex), [github](https://github.com/nonsequitur/smex)
* use-package: [melpa](https://melpa.org/#/use-package), [github](https://github.com/jwiegley/use-package)
* web-mode: [melpa](https://melpa.org/#/web-mode), [github](https://github.com/fxbois/web-mode)
* yaml-mode: [melpa](https://melpa.org/#/yaml-mode), [github](https://github.com/yoshiki/yaml-mode)
* yasnippet: [gnu](http://elpa.gnu.org/packages/yasnippet.html), [melpa](https://melpa.org/#/yasnippet), [github](https://github.com/capitaomorte/yasnippet)
* w3m: [melpa](https://melpa.org/#/w3m), [github](https://github.com/emacs-w3m/emacs-w3m)

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
