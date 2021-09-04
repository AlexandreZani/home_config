;; load local emacs config if it exists.
(let ((local-emacs-config "~/.local.conf/emacs.el"))
     (when (file-exists-p local-emacs-config)
       (load local-emacs-config)))

;; Add the melpa packages
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(setq package-selected-packages '(lean-mode fstar-mode))

(let ((need-to-install nil))
  (dolist (p package-selected-packages)
    (unless (require p nil t)
      (setq need-to-install t)
      (print (concat "need to install: " (symbol-name p)))
      )
    )
  (when need-to-install
    (package-refresh-contents)
    (package-install-selected-packages)))

(package-autoremove)

(show-paren-mode 1)

;; Add the submodule packages
(setq submodule-packages '(google-c-style clang-format))

(dolist (p submodule-packages)
  (add-to-list 'load-path (concat "~/.emacs.d/" (symbol-name p))))
(dolist (p submodule-packages)
  (require p))

;; display line numbers
(global-display-line-numbers-mode)

;; disable bell.
(setq ring-bell-function 'ignore)
(setq visible-bell nil)

;; stop creating backup~ files.
(setq make-backup-files nil)

;; stop creating #auto-save# files.
(setq auto-save-default nil)

;; stop showing the welcome buffer
(setq inhibit-startup-screen t)

;; format c++ files on save.
(defun c++-mode-before-save-hook ()
  (when (eq major-mode 'c++-mode)
    (clang-format-buffer)))
(add-hook 'before-save-hook #'c++-mode-before-save-hook)
