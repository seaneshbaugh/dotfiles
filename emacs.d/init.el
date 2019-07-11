;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(require 'cask "/usr/local/share/emacs/site-lisp/cask/cask.el")

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

(setq whitespace-action '(auto-cleanup))

;; Always remove trailing whitepaace on save
(add-hook 'before-save-hook
          (lambda ()
            (unless (string= (file-name-nondirectory (buffer-file-name)) "structure.sql")
              (delete-trailing-whitespace))))

;; Show line numbers
(global-display-line-numbers-mode t)

;; Add a space between line numbers and the buffer
(setq display-line-numbers "%4%d ")

;; Enable copy and pasting from clipboard
(setq x-select-enable-clipboard t)

;; Show column number
(setq column-number-mode t)

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

;; Function to remove magic comment insert hook
(defun remove-enh-magic-comment ()
  (remove-hook 'before-save-hook 'enh-ruby-mode-set-encoding t))

;; Add the hook to call our hook removing function
(add-hook 'enh-ruby-mode-hook 'remove-enh-magic-comment)

(setq ruby-insert-encoding-magic-comment nil)

(add-hook 'enh-ruby-mode-hook #'rubocop-mode)

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

;; Coffee mode config.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(coffee-tab-width 2)
 '(js-indent-level 2)
 '(neo-window-width 50)
 '(safe-local-variable-values (quote ((encoding . utf-8))))
 '(send-mail-function (quote smtpmail-send-it)))

;; Org mode config.
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(add-hook 'org-mode-hook 'turn-on-font-lock)
(global-set-key "\C-c l" 'org-store-link)
(global-set-key "\C-c a" 'org-agenda)
(global-set-key "\C-c b" 'org-iswitchb)

;; SCSS mode config.
(setq scss-compile-at-save nil)

;; Enable git-gutter mode
(global-git-gutter-mode t)
;;(git-gutter:linum-setup)

;; Enable neotree
(require 'neotree)
(global-set-key [f8] 'neotree-toggle)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(neo-file-link-face ((t (:foreground "brightwhite")))))

(global-set-key (kbd "C-x p") 'previous-multiframe-window)

(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)

(setq w3m-use-cookies t)
