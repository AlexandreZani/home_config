;; Disable bell.
(setq ring-bell-function 'ignore)

;; stop creating backup~ files.
(setq make-backup-files nil)

;; stop creating #auto-save# files.
(setq auto-save-default nil)

;; stop showing the welcome buffer
(setq inhibit-startup-screen t)

;; Enable google c style
(add-to-list 'load-path "~/.emacs.d/google-c-style")
(require 'google-c-style)
(add-hook 'c-mode-common-hook 'google-set-c-style)

;; Enable clang-format on save
(add-to-list 'load-path "~/.emacs.d/clang-format")
(require 'clang-format)

(defun c++-mode-before-save-hook ()
  (when (eq major-mode 'c++-mode)
    (clang-format-buffer)))

(add-hook 'before-save-hook #'c++-mode-before-save-hook)
