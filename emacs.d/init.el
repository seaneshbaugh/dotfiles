(setq custom-file "~/.emacs.d/custom.el")
(ignore-errors (load custom-file))

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package-ensure)
(setq use-package-always-ensure t)

(use-package try)

(use-package which-key
	     :config
	     (which-key-mode))
