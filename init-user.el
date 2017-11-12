;;; init-user.el --- Customizations on base emacs.d
;;
;;; Commentary:
;;; Code:


;; Theme: monokai.el
(use-package monokai-theme)


;; Enable smex -- for M-x autocomplete
(use-package smex)
(global-set-key (kbd "M-x") 'smex)


;; ido mode -- autocomplete C-x b / C-x C-f
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)


;; Enable recentf
(use-package recentf)
(recentf-mode t)

;; replace 'find-file-read-only'
(global-set-key (kbd "C-x C-r") 'ido-recentf-open)

(setq recentf-max-menu-items 15)
(setq recentf-max-saved-items 500)
(setq recentf-auto-cleanup 'never)

(defun ido-recentf-open ()
  "Use 'ido-completing-read' to \\[find-file] a recent file."
  (interactive)
  (if (find-file (ido-completing-read "Find recent file: " recentf-list))
      (message "Opening file...")
    (message "Aborting")))


;; Golang / gotest
(use-package go-mode)
(use-package gotest)
(define-key go-mode-map (kbd "C-x t") 'go-test-current-test)
(define-key go-mode-map (kbd "C-x f") 'go-test-current-file)


;; Projectile
(projectile-mode t)
(global-set-key (kbd "s-t") 'projectile-find-file)
(global-set-key (kbd "s-g") 'projectile-grep)
(global-set-key (kbd "s-p") 'projectile-switch-project)


;; Show full file path in the title bar
(setq
 frame-title-format
 '((:eval (if (buffer-file-name)
        (abbreviate-file-name (buffer-file-name))
      "%b"))))


;; Disables audio bell
(setq ring-bell-function
      (lambda () (message "*beep*")))


;; Highlight current line
(global-hl-line-mode)


;; Scroll one line at a time
(setq scroll-conservatively 10)


;; Remove bars
(menu-bar-mode -1)
(tool-bar-mode -1)
(toggle-scroll-bar -1)


;; Remove selected text via backspace / enter
(delete-selection-mode t)


;; Use y/n instead of full yes/no
(fset 'yes-or-no-p 'y-or-n-p)


;; Line numbers
(global-linum-mode t)


;; Remove trailing whitespace before saving
(add-hook 'before-save-hook 'delete-trailing-whitespace)


;; Git status
(global-set-key (kbd "C-c g") 'magit-status)


;; Enable org mode
(use-package org-mode)


;; Enable undo-tree
(use-package undo-tree)
(global-undo-tree-mode t)
(global-set-key (kbd "s-Z") 'undo-tree-redo)


;; Enable flycheck
(use-package flycheck)
(global-flycheck-mode t)


(provide 'init-user)
;;; init-user.el ends here
