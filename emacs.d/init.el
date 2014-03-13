(add-to-list 'load-path "~/.emacs.d/")

(require 'cask "~/.cask/cask.el")

(cask-initialize)

(require 'pallet)

(require 'dash)
(require 'f)
(require 'git)
(require 's)
(require 'use-package)

(defun load-x (file)
  (load (f-expand file user-emacs-directory)))

(let ((default-directory user-emacs-directory))
  (load-x "aliases"))

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

(setq enh-ruby-program "~/.rvm/rubies/ruby-2.0.0-p247/bin/ruby")
(autoload 'enh-ruby-mode "enh-ruby-mode" "Major mode for ruby files" t)
(add-to-list 'auto-mode-alist '("\\.rb$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("\\.ru$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile.lock$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("Capfile$" . enh-ruby-mode))
(add-to-list 'interpreter-mode-alist '("ruby" . enh-ruby-mode))
(setq enh-ruby-bounce-deep-indent t)
(setq enh-ruby-hanging-brace-indent-level 2)

(load "algol68-mode")
(load "io-mode")

(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/.cask/24.3.1/elpa/auto-complete-20130724.1750/dict")
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

(load "custom-commands")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("9773ddae50e6891ae5cebb0f8c5ea099eba19f6dd1d31efe3328166a5fadb527" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(load-theme 'sean t)