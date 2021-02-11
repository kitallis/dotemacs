(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)

;; ======
;; THEMES
;; ======

(use-package monokai-pro-theme
  :disabled t)

(use-package moe-theme
  :disabled t
  :config
  (setq moe-theme-highlight-buffer-id t
        show-paren-style 'parenthesis)
  (moe-theme-set-color 'blue)
  (moe-dark))
(use-package srcery-theme
  :disabled t)

(use-package nord-theme
  :disabled t)

(use-package darkokai-theme
  :config
  (setq darkokai-mode-line-padding 1))

(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (load-theme 'doom-challenger-deep))

(use-package abyss-theme
  :disabled t)

(use-package gruvbox-theme
  :disabled t)

(use-package dracula-theme
  :disabled t
  :config
  (load-theme 'dracula))

(use-package bubbleberry-theme
  :disabled t
  :config
  (load-theme 'bubbleberry))

(use-package solarized-theme
  :init
  :disabled t
  (setq solarized-distinct-fringe-background nil)
  (setq solarized-use-less-bold t)
  (setq solarized-use-variable-pitch nil)
  (setq x-underline-at-descent-line t)
  :config
  (load-theme 'solarized-dark))

(use-package expand-region
  :config (global-set-key (kbd "C-;") 'er/expand-region))

(defun load-local (file)
  "Load local .el files"
  (load (f-expand file user-emacs-directory)))


;; ====
;; CODE
;; ====

(load-local "init-code")


;; =====
;; EFUNS
;; =====

(load-local "init-efuns")


;; ==================
;; GLOBAL KEYBINDINGS
;; ==================

(global-set-key (kbd "s-g") 'kg/search-marked-region-if-available)
(global-set-key (kbd "s-l") 'goto-line)
(global-set-key (kbd "s-t") 'projectile-find-file)
(global-set-key (kbd "M-j") (lambda () (interactive) (join-line -1)))
(global-set-key (kbd "s-{") 'previous-buffer)
(global-set-key (kbd "s-}") 'next-buffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x 3") 'kg/split-right-and-move)
(global-set-key (kbd "C-x 2") 'kg/split-below-and-move)
(global-set-key (kbd "C-a") 'kg/beginning-of-line-dwim)
(global-set-key [(meta shift down)] 'kg/duplicate-start-of-line-or-region)
(global-set-key (kbd "C-c C-l") 'org-capture)
(global-set-key (kbd "<f6>") 'kg/show-user-config)
(global-set-key (kbd "s-w") 'kill-this-buffer)
(global-set-key (kbd "<s-S-return>") 'kg/toggle-maximize-buffer)


;; ================
;; ADD THEME FOLDER
;; ================

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
