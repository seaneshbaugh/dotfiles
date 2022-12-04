(setq custom-file "~/.emacs.d/custom.el")
(ignore-errors (load custom-file))

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

(use-package s)

(use-package a68-mode
  :straight '(a68-mode :host github
		       :repo "omar-polo/a68-mode"
		       :branch "main"))

(use-package clojure-mode)

(use-package cobol-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.cob$" . cobol-mode))
  (add-to-list 'auto-mode-alist '("\\.cbl\\'" . cobol-mode))
  (add-to-list 'auto-mode-alist '("\\.cpy\\'" . cobol-mode)))

(use-package coffee-mode)

(use-package csharp-mode
  :hook (csharp-mode . electric-pair-local-mode))

(use-package d-mode)

(use-package dired
  :straight nil
  :ensure nil
  :config
  (setq dired-use-ls-dired nil))

(use-package dockerfile-mode)

(use-package elixir-mode
  :hook (elixir-mode . (lambda () (add-hook 'before-save-hook 'elixir-format nil t))))

(use-package enh-ruby-mode
  :config
  (setq ruby-insert-encoding-magic-comment nil)
  (setq enh-ruby-program (s-chomp (shell-command-to-string "which ruby")))
  (setq enh-ruby-bounce-deep-indent t)
  (setq enh-ruby-hanging-brace-indent-level 2)
  :init
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
  (add-to-list 'interpreter-mode-alist '("ruby" . enh-ruby-mode)))

(use-package erlang)

(use-package ess)

(use-package fsharp-mode)

(use-package git-gutter
  :init
  (global-git-gutter-mode t))

(use-package go-mode)

(use-package groovy-mode)

(use-package haml-mode)

(use-package handlebars-mode
  :custom
  (handlebars-basic-offset 4))

(use-package haskell-mode)

(use-package io-mode)

(use-package julia-mode)

(use-package js
  :straight nil
  :ensure nil
  :custom
  (js-indent-level 2))

(use-package less-css-mode)

(use-package lua-mode
  :custom
  (lua-indent-level 2))

(use-package magit
  :if
  (executable-find "git")
  :commands magit-status)

(use-package markdown-mode)

(use-package mood-line
  :init
  (mood-line-mode))

(use-package neotree
  :init
  (global-set-key [f8] 'neotree-toggle)
  :custom
  (neo-window-width 50)
  :custom-face
  (neo-file-link-face ((t (:foreground "brightwhite")))))

(use-package nginx-mode)

(use-package nim-mode)

(when (string-equal system-type "darwin")
  (use-package pbcopy
    :init
    (turn-on-pbcopy)))

(use-package rjsx-mode
  :init
  ;; Use rjsx-mode for any JavaScript files in a directory named components or containers.
  (add-to-list 'auto-mode-alist '("components\\/.*\\.js\\'" . rjsx-mode))
  (add-to-list 'auto-mode-alist '("containers\\/.*\\.js\\'" . rjsx-mode)))

(use-package rubocop)

(use-package rust-mode)

(use-package sass-mode)

(use-package scala-mode)

(use-package sgml-mode
  :straight nil
  :config
  (setq sgml-basic-offset 4))

(use-package slim-mode)

(use-package which-key
  :config
  (which-key-mode))

(use-package w3m
  :config
  (setq w3m-use-cookies t))

(use-package yaml-mode)

;; Backups

;; Don't clutter directories with backup files, put them all in .emacs.d directory.
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))

;; Silently delete execess backup versions.
(setq delete-old-versions t)

;; Only keep the last 1000 backups of a file.
(setq kept-old-versions 1000)

;; Even version controlled files will be backed up.
(setq vc-make-backup-files t)

;; Use version numbers for backup files.
(setq version-control t)

;; Display

;; Add a space between line numbers and the buffer.
(setq display-line-numbers "%4%d ")

;; Show line numbers.
(global-display-line-numbers-mode t)

;; Show column number.
(setq column-number-mode t)

;; Show trailing whitespace.
(setq-default show-trailing-whitespace t)

;; Editing

;; Indent with spaces instead of tabs.
(setq-default indent-tabs-mode nil)

;; Automatically clean up bad whitespace.
(setq whitespace-action '(auto-cleanup))

;; Always remove trailing whitepaace on save unless the file is named
;; "structure.sql". For some reason Rails structure.sql files seem to
;; sometimes have trailing whitespace which when removed causes
;; problems.
(add-hook 'before-save-hook
          (lambda ()
            (unless (string= (file-name-nondirectory (buffer-file-name)) "structure.sql")
              (delete-trailing-whitespace))))

;; Functions

;; https://rejeep.github.io/emacs/elisp/2010/03/11/duplicate-current-line-or-region-in-emacs.html
(defun duplicate-current-line-or-region (arg)
  "Duplicates the current line or region ARG times.
If there's no region, the current line will be duplicated. However, if
there's a region, all lines that region covers will be duplicated."
  (interactive "p")
  (let (beg end (origin (point)))
    (if (and mark-active (> (point) (mark)))
        (exchange-point-and-mark))
    (setq beg (line-beginning-position))
    (if mark-active
        (exchange-point-and-mark))
    (setq end (line-end-position))
    (let ((region (buffer-substring-no-properties beg end)))
      (dotimes (i arg)
        (goto-char end)
        (newline)
        (insert region)
        (setq end (point)))
      (goto-char (+ origin (* (length region) arg) arg)))))

;; Keybindings

(global-set-key (kbd "C-x p") 'previous-multiframe-window)
(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)
(global-set-key (kbd "M-D") 'duplicate-current-line-or-region)

;; Aliases

;; Alias y/n for yes/no.
(defalias 'yes-or-no-p 'y-or-n-p)

;; Terminal

;; This fixes some rendering issues with zsh.
(setq system-uses-terminfo nil)

;; Turn off the bell noise.
(setq visible-bell 1)

;; Locale

;; Use UTF-8 when reading text from files and other sources.
(set-language-environment 'utf-8)

;; Use UTF-8 for decoding system error message.
(setq locale-coding-system 'utf-8)

;; Use UTF-8 for keyboard input.
(set-keyboard-coding-system 'utf-8)

;; Use UTF-8 when accepting and sending selected text.
(set-selection-coding-system 'utf-8)

;; Use UTF-8 as the preferred encoding for saving buffers.
(prefer-coding-system 'utf-8)

;; Use UTF-8 for terminal output.
(set-terminal-coding-system 'utf-8)
