# dotfiles

My dotfiles to set up my environment. I mostly use OSX but these should generally work on Linux with a little modification.

You should probably make your own.

## Installation

    $ git clone git@github.com:seaneshbaugh/dotfiles.git
    $ cd ~/dotfiles
    $ ./install.sh

## Notes

This is based off [Ryan Bates' old dotfile setup](https://github.com/ryanb/dotfiles/tree/custom-bash-zsh). [oh-my-zsh](https://github.com/robbyrussell/oh-my-zsh) is great, but is just a bit much. I like to keep my setup as close to the default as possible and change only what I know from personal experience the things I prefer.

## Emacs Config Stuff

### ELPA Sources

The following ELPA sources are used:

* [gnu](http://elpa.gnu.org/packages/)
* [melpa](http://melpa.org/#/)
* [marmalade](http://marmalade-repo.org/packages/)
* [org](http://orgmode.org/elpa/)

### ELPA Packages

The following ELPA packages are used:

* auto-complete: [melpa](http://melpa.org/#/auto-complete), [marmalade](https://marmalade-repo.org/packages/auto-complete), [github](https://github.com/auto-complete/auto-complete)
* bind-key: [melpa](http://melpa.org/#/bind-key), [github](https://github.com/jwiegley/use-package)
* cask: [melpa](http://melpa.org/#/cask), [github](https://github.com/cask/cask)
* clojure-mode: [melpa](http://melpa.org/#/clojure-mode), [marmalade](https://marmalade-repo.org/packages/clojure-mode), [github](https://github.com/clojure-emacs/clojure-mode)
* coffee-mode: [melpa](http://melpa.org/#/coffee-mode), [marmalade](https://marmalade-repo.org/packages/coffee-mode), [github](https://github.com/defunkt/coffee-mode)
* csharp-mode: [melpa](http://melpa.org/#/csharp-mode), [marmalade](https://marmalade-repo.org/packages/csharp-mode), [github](https://github.com/josteink/csharp-mode)
* d-mode: [melpa](http://melpa.org/#/d-mode), [marmalade](https://marmalade-repo.org/packages/d-mode), [github](https://github.com/Emacs-D-Mode-Maintainers/Emacs-D-Mode)
* dash: [melpa](http://melpa.org/#/dash), [marmalade](https://marmalade-repo.org/packages/dash), [github](https://github.com/magnars/dash.el)
* drag-stuff: [melpa](http://melpa.org/#/drag-stuff), [marmalade](https://marmalade-repo.org/packages/drag-stuff), [github](https://github.com/rejeep/drag-stuff.el)
* elixir-mode: [melpa](http://melpa.org/#/elixir-mode), [marmalade](https://marmalade-repo.org/packages/elixir-mode), [github](https://github.com/elixir-lang/emacs-elixir)
* enh-ruby-mode: [melpa](http://melpa.org/#/enh-ruby-mode), [github](https://github.com/zenspider/enhanced-ruby-mode)
* erlang: [melpa](http://melpa.org/#/erlang), [marmalade](https://marmalade-repo.org/packages/erlang), [source](http://www.erlang.org/download/contrib/erlang.el)
* ess: [melpa](http://melpa.org/#/ess), [marmalade](https://marmalade-repo.org/packages/ess), [github](https://github.com/emacs-ess/ESS)
* exec-path-from-shell: [melpa](http://melpa.org/#/exec-path-from-shell), [marmalade](https://marmalade-repo.org/packages/exec-path-from-shell), [github](https://github.com/purcell/exec-path-from-shell)
* expand-region: [melpa](http://melpa.org/#/expand-region), [marmalade](https://marmalade-repo.org/packages/expand-region), [github](https://github.com/magnars/expand-region.el)
* f: [melpa](http://melpa.org/#/f), [marmalade](https://marmalade-repo.org/packages/f), [github](https://github.com/rejeep/f.el)
* flycheck: [melpa](http://melpa.org/#/flycheck), [marmalade](https://marmalade-repo.org/packages/flycheck), [github](https://github.com/flycheck/flycheck)
* flycheck-cask: [melpa](http://melpa.org/#/flycheck-cask), [marmalade](https://marmalade-repo.org/packages/flycheck-cask), [github](https://github.com/flycheck/flycheck-cask)
* fsharp-mode: [melpa](http://melpa.org/#/fsharp-mode), [github](https://github.com/rneatherway/emacs-fsharp-mode-bin)
* go-mode: [melpa](http://melpa.org/#/go-mode), [marmalade](https://marmalade-repo.org/packages/go-mode), [github](https://github.com/dominikh/go-mode.el)
* groovy-mode: [melpa](http://melpa.org/#/groovy-mode), [marmalade](https://marmalade-repo.org/packages/groovy-mode), [github](https://github.com/Groovy-Emacs-Modes/groovy-emacs-modes)
* haml-mode: [melpa](http://melpa.org/#/haml-mode), [marmalade](https://marmalade-repo.org/packages/haml-mode), [github](https://github.com/nex3/haml-mode)
* haskell-mode: [melpa](http://melpa.org/#/haskell-mode), [marmalade](https://marmalade-repo.org/packages/haskell-mode), [github](https://github.com/haskell/haskell-mode)
* htmlize: [melpa](http://melpa.org/#/htmlize), [marmalade](https://marmalade-repo.org/packages/htmlize), [source](http://fly.srk.fer.hr/~hniksic/emacs/htmlize.el.cgi)
* idle-highlight-mode: [melpa](http://melpa.org/#/idle-highlight-mode), [marmalade](https://marmalade-repo.org/packages/idle-highlight-mode), [github](https://github.com/nonsequitur/idle-highlight-mode)
* less-css-mode: [melpa](http://melpa.org/#/less-css-mode), [marmalade](https://marmalade-repo.org/packages/less-css-mode), [github](https://github.com/purcell/less-css-mode)
* lua-mode: [melpa](http://melpa.org/#/lua-mode), [marmalade](https://marmalade-repo.org/packages/lua-mode), [github](https://github.com/immerrr/lua-mode)
* magit: [melpa](http://melpa.org/#/magit), [marmalade](https://marmalade-repo.org/packages/magit), [github](https://github.com/magit/magit/tree/master)
* multiple-cursors: [melpa](http://melpa.org/#/multiple-cursors) ([marmalade](https://marmalade-repo.org/packages/multiple-cursors), [github](https://github.com/magnars/multiple-cursors.el))
* nim-mode: [melpa](http://melpa.org/#/nim-mode), [github](https://github.com/reactormonk/nim-mode)
* no-easy-keys: [marmalade](https://marmalade-repo.org/packages/no-easy-keys), [github](https://github.com/danamlund/emacs-no-easy-keys)
* nyan-mode: [melpa](http://melpa.org/#/nyan-mode), [github](https://github.com/TeMPOraL/nyan-mode)
* org-plus-contrib: [org](http://orgmode.org/elpa/), [source](http://orgmode.org/cgit.cgi/org-mode.git/)
* pallet: [melpa](http://melpa.org/#/pallet), [github](https://github.com/rdallasgray/pallet)
* popwin: [melpa](http://melpa.org/#/popwin), [marmalade](https://marmalade-repo.org/packages/popwin), [github](https://github.com/m2ym/popwin-el)
* prodigy: [melpa](http://melpa.org/#/prodigy), [github](https://github.com/rejeep/prodigy.el)
* projectile: [melpa](http://melpa.org/#/projectile), [marmalade](https://marmalade-repo.org/packages/projectile), [github](https://github.com/bbatsov/projectile)
* rust-mode: [melpa](http://melpa.org/#/rust-mode), [marmalade](https://marmalade-repo.org/packages/rust-mode), [github](https://github.com/rust-lang/rust-mode)
* s: [melpa](http://melpa.org/#/s), [marmalade](https://marmalade-repo.org/packages/s), [github](https://github.com/magnars/s.el)
* sass-mode: [melpa](http://melpa.org/#/sass-mode), [marmalade](https://marmalade-repo.org/packages/sass-mode), [github](https://github.com/nex3/sass-mode)
* scala-mode2: [melpa](http://melpa.org/#/scala-mode2), [github](https://github.com/hvesalai/scala-mode2)
* smartparens: [melpa](http://melpa.org/#/smartparens), [github](https://github.com/Fuco1/smartparens)
* smex: [melpa](http://melpa.org/#/smex), [marmalade](https://marmalade-repo.org/packages/smex), [github](https://github.com/nonsequitur/smex)
* use-package: [melpa](http://melpa.org/#/use-package), [github](https://github.com/jwiegley/use-package)
* web-mode: [melpa](http://melpa.org/#/web-mode), [marmalade](https://marmalade-repo.org/packages/web-mode), [github](https://github.com/fxbois/web-mode)
* yaml-mode: [melpa](http://melpa.org/#/yaml-mode), [marmalade](https://marmalade-repo.org/packages/yaml-mode), [github](https://github.com/yoshiki/yaml-mode)
* yasnippet: [gnu](http://elpa.gnu.org/packages/yasnippet.html), [melpa](http://melpa.org/#/yasnippet), [marmalade](https://marmalade-repo.org/packages/yasnippet), [github](https://github.com/capitaomorte/yasnippet)

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
