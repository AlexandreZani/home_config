;; load local emacs config if it exists.
(let ((local-emacs-config "~/.local.conf/emacs.el"))
     (when (file-exists-p local-emacs-config)
       (load local-emacs-config)))

;; display line numbers
(global-display-line-numbers-mode)

;; disable bell.
(setq ring-bell-function 'ignore)

;; stop creating backup~ files.
(setq make-backup-files nil)

;; stop creating #auto-save# files.
(setq auto-save-default nil)

;; stop showing the welcome buffer
(setq inhibit-startup-screen t)

;; Add the submodules
(setq submodule-packages '(google-c-style
			   clang-format
			   ;; dependencies for lean-mode
			   dash
			   f
			   flycheck
			   s
			   lean-mode
			   ))

(dolist (p submodule-packages)
  (add-to-list 'load-path (concat "~/.emacs.d/" (symbol-name p))))


;; format c++ files on save.
(defun c++-mode-before-save-hook ()
  (when (eq major-mode 'c++-mode)
    (clang-format-buffer)))
(add-hook 'before-save-hook #'c++-mode-before-save-hook)
