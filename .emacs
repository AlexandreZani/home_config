;; stop creating backup~ files.
(setq make-backup-files nil)

;; stop creating #auto-save# files.
(setq auto-save-default nil)

;; Enable google c style
(add-to-list 'load-path "~/.emacs.d/google-c-style")
(require 'google-c-style)
(add-hook 'c-mode-common-hook 'google-set-c-style)
