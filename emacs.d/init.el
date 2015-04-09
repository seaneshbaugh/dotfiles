(require 'cask "/usr/local/share/emacs/site-lisp/cask.el")

;; Load dependencies defined in "Cask" file
(cask-initialize)

;; Enable pallet (https://github.com/rdallasgray/pallet#installation)
(require 'pallet)
(pallet-mode t)

;; Do not make backup files
(setq make-backup-files nil)

;; Prefer utf-8 encoding
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Use Emacs terminfo, not system terminfo
(setq system-uses-terminfo nil)

;; Show trailing whitespace
(setq-default show-trailing-whitespace t)

;; Indent with spaces instead of tabs
(setq-default indent-tabs-mode nil)

;; Show line numbers
(global-linum-mode t)

;; Add a space between line numbers and the buffer
(setq linum-format "%d ")

;; Enable copy and pasting from clipboard
(setq x-select-enable-clipboard t)

;; Get rid of "ls does not support --dired" message on OSX
(setq dired-use-ls-dired nil)

;; Define function to load files in "emacs.d" directory
(defun load-x (file)
  (load (f-expand file user-emacs-directory)))

;; Load custom aliases
(load-x "aliases")

;; Load custom commands
(load-x "custom-commands")

;; Load algol68-mode
(load-x "algol68-mode")

;; Load Io mode
(load-x "io-mode")

(autoload 'enh-ruby-mode "enh-ruby-mode" "Major mode for ruby files" t)

;; Make sure all Ruby-related files use enh-ruby-mode
(add-to-list 'auto-mode-alist '("\\.rb$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("\\.ru$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile.lock$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("Capfile$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("Thorfile$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("\\.thor$" . enh-ruby-mode))

(add-to-list 'interpreter-mode-alist '("ruby" . enh-ruby-mode))

;; Set enh-ruby-mode's Ruby to whatever the current Ruby bin is
(setq enh-ruby-program (s-chomp (shell-command-to-string "which ruby")))
(setq enh-ruby-bounce-deep-indent t)
(setq enh-ruby-hanging-brace-indent-level 2)

;; No Easy Keys
(require 'no-easy-keys)
(no-easy-keys 1)

;; Enable OSX clipboard copying
(require 'pbcopy)
(turn-on-pbcopy)

(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/dotfiles/emacs.d/.cask/24.4.1/elpa/auto-complete-20150322.813")
(ac-config-default)
(setq ac-ignore-case nil)
(dolist (mode '(emacs-lisp-mode
                enh-ruby-mode
                java-mode
                javascript-mode
                html-mode
                list-mode
                org-mode
                sh-mode
                text-mode
                web-mode))
 (add-to-list 'ac-modes mode))

;; Org mode config.
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(add-hook 'org-mode-hook 'turn-on-font-lock)
(global-set-key "\C-c l" 'org-store-link)
(global-set-key "\C-c a" 'org-agenda)
(global-set-key "\C-c b" 'org-iswitchb)
