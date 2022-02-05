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

(use-package a68-mode
  :straight '(a68-mode :host github
		       :repo "omar-polo/a68-mode"
		       :branch "main"))

(use-package clojure-mode)

(use-package cobol-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.cob$" . cobol-mode)))

(use-package git-gutter
  :init
  (global-git-gutter-mode t))

(use-package handlebars-mode
  :custom
  (handlebars-basic-offset 4 "The basic indentation offset for handlebars."))

(use-package neotree
  :init
  (global-set-key [f8] 'neotree-toggle)
  :custom
  (neo-window-width 50 "Specifies the width of the NeoTree window.")
  :custom-face
  (neo-file-link-face ((t (:foreground "brightwhite")))))

(use-package which-key
  :config
  (which-key-mode))

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
