;; load local emacs config if it exists.
(let ((local-emacs-config "~/.local.conf/emacs.el"))
     (when (file-exists-p local-emacs-config)
       (load local-emacs-config)))

;; Add the melpa packages
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(setq package-selected-packages '(lean-mode
				  fstar-mode
				  proof-general
				  company-coq
				  markdown-mode
				  scad-mode))

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

(custom-set-variables
 '(markdown-command "/usr/bin/pandoc"))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(fstar-subp-overlay-processed-face ((((background light)) (:background "#EAF8FF" :extend t)) (((background dark)) (:box nil)))))

(if (executable-find "agda-mode")
    (load-file (let ((coding-system-for-read 'utf-8))
                 (shell-command-to-string "agda-mode locate"))))

;; auto-load agda-mode for .agda and .lagda.md
(setq auto-mode-alist
   (append
     '(("\\.agda\\'" . agda2-mode)
       ("\\.lagda.md\\'" . agda2-mode))
     auto-mode-alist))
