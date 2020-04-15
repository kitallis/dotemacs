(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)

(set-face-attribute 'default nil
                    :family "Office Code Pro"
                    :height 130
                    :weight 'normal
                    :width 'normal)

(setq-default line-spacing 0.1
              help-window-select t
              truncate-lines t)

;; workaround for alt not working as meta key
(setq mac-option-key-is-meta nil
      mac-command-key-is-meta t
      mac-command-modifier 'super
      mac-option-modifier 'meta
      inhibit-splash-screen t
      inhibit-startup-message t
      ring-bell-function 'ignore
      select-enable-clipboard t
      save-interprogram-paste-before-kill t
      backup-inhibited t
      make-backup-files nil
      auto-save-default nil
      vc-handled-backends '(Git)
      default-file-name-coding-system 'utf-8
      buffer-file-coding-system 'utf-8
      org-ellipsis " ▶")

;; (set-default 'cursor-type '(bar . 2))
(set-default 'cursor-type 'box)
(blink-cursor-mode 0)

(global-hl-line-mode nil)
(set-face-attribute hl-line-face nil :underline nil)

;; Rewrite selected text
(delete-selection-mode 1)

(setq org-todo-keywords
      '((sequence "TODO" "DOING" "|" "DONE")))

;; ======
;; THEMES
;; ======

(use-package monokai-pro-theme :disabled t)
(use-package moe-theme
  :disabled t
  :config
  (setq moe-theme-highlight-buffer-id t
        show-paren-style 'parenthesis)
  (moe-theme-set-color 'blue)
  (moe-dark))
(use-package srcery-theme :disabled t)
(use-package nord-theme :disabled t)
(use-package darkokai-theme
  :disabled t
  :config
  (setq darkokai-mode-line-padding 1))
(use-package doom-themes
  :disabled t
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (load-theme 'doom-gruvbox))
(use-package abyss-theme :disabled t)
(use-package gruvbox-theme)

;; (load-theme 'monokai)

;; ==========================
;; OTHER THIRD PARTY PACKAGES
;; ==========================

(use-package expand-region
  :config (global-set-key (kbd "C-;") 'er/expand-region))

(use-package ido-vertical-mode
  :config
  (ido-vertical-mode 1)
  (setq ido-use-faces t
        ido-vertical-show-count t
        ido-vertical-define-keys 'C-n-and-C-p-only))

(use-package ivy
  :bind ("s-b". 'ivy-switch-buffer)
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-height 10)
  (setq ivy-count-format "")
  (setq ivy-count-format "(%d/%d) ")

  ;; Use [Enter] to navigate into the directory, not dired-open it.
  (define-key ivy-minibuffer-map (kbd "C-m") 'ivy-alt-done)

  ;; (global-set-key (kbd "s-b") 'ivy-switch-buffer)
  ;; (use-package flx
  ;;   :init
  ;;   (setq ivy-re-builders-alist '((t . ivy--regex-fuzzy))))
  )

(use-package counsel
  :init (use-package rg
          :config
          (setq rg-command-line-flags '("-w"))
          (setq rg-ignore-case 'smart))
  :config
  ;; (global-set-key (kbd "s-g") 'counsel-rg)
  (global-set-key (kbd "C-x C-r") 'counsel-recentf)
  (setq counsel-rg-base-command "rg -i -w --no-heading --line-number %s .")
  (setq recentf-max-saved-items 50)
  (setq recentf-auto-cleanup (* 24 60 60)))

(use-package swiper
  :config
  (global-set-key (kbd "s-f") 'swiper))

(use-package org
  :config
  (setq org-directory "~/Box Sync/org-notes"
        org-default-notes-file (concat org-directory "/notes.txt")
        org-export-coding-system 'utf-8
        org-ellipsis " ▼ "
        org-startup-indented nil
        org-src-fontify-natively t
        org-src-preserve-indentation t)
  (setq org-capture-templates
        '(
          ("l" "Add a link to the linklog" entry
           (file+olp+datetree (lambda () (concat org-directory "/linklog.org")))
           "**** %?")
          ))
  (add-hook 'org-mode-hook
            (lambda () (when (fboundp 'org-mac-grab-link)
                         (load-file "~/.emacs.d/org-mac-link.el")))))

(use-package multiple-cursors
  :config
  (setq-default mc/edit-lines-empty-lines 'ignore
                mc/insert-numbers-default 1)
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this))

(use-package doom-modeline
  :hook (after-init . doom-modeline-init)
  :init
  (set-face-attribute 'mode-line nil :height 130)
  (set-face-attribute 'mode-line-inactive nil :height 130)
  :config
  (setq doom-modeline-buffer-file-name-style 'relative-from-project
        doom-modeline-major-mode-icon nil
        doom-modeline-minor-modes nil
        doom-modeline-github nil
        doom-modeline-version nil
        doom-modeline-height 10
        doom-modeline-bar-width 1
        doom-modeline-buffer-encoding nil
        doom-modeline-vcs-max-length 50
        doom-modeline-gnus nil
        doom-modeline-irc nil
        doom-modeline-persp-name nil))

(use-package undo-tree
  :bind ("s-Z" . 'undo-tree-redo)
  :config
  (global-undo-tree-mode))

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "pandoc")
  :config
  (add-hook 'gfm-mode-hook 'linum-mode)
  (add-hook 'markdown-mode-hook 'linum-mode))

(use-package smartparens
  :init
  (add-hook 'markdown-mode-hook 'smartparens-mode))

(use-package hl-todo
  :config
  (global-hl-todo-mode t))

(use-package yaml-mode)
(use-package json-mode)

(use-package counsel-projectile
  :config
  (counsel-projectile-mode))

(use-package rust-mode
  :config
  (add-hook 'rust-mode-hook #'flycheck-mode)
  (add-hook 'rust-mode-hook #'smartparens-mode)
  (add-hook 'rust-mode-hook
            (lambda ()
              (local-set-key (kbd "C-c <tab>") #'rust-format-buffer)))

  (use-package racer
    :init
    (setq racer-rust-src-path
          "/Users/kiran/.rustup/toolchains/stable-x86_64-apple-darwin/lib/rustlib/src/rust/src")
    :config
    (add-hook 'rust-mode-hook #'racer-mode)
    (add-hook 'racer-mode-hook #'eldoc-mode)
    (add-hook 'racer-mode-hook #'company-mode)
    (define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)
    (setq company-tooltip-align-annotations t))

  (use-package cargo
    :config (add-hook 'rust-mode-hook 'cargo-minor-mode))

  (use-package flycheck-rust
    :config (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)))

(use-package fast-scroll
  :config
  (fast-scroll-config)
  (fast-scroll-mode 1))

(use-package flycheck-clj-kondo
  :config
  (add-hook 'clojure-mode-hook #'flycheck-mode))

;; (use-package ivy-posframe
;;   :after ivy
;;   :diminish
;;   :config
;;   (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-top-center))
;;         ;; ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-center))
;;         ivy-posframe-height-alist '((t . 20))
;;         ivy-posframe-parameters '((internal-border-width . 10)))
;;   (setq ivy-posframe-width 70)
;;   (ivy-posframe-mode -1))

;; (use-package highlight-indent-guides
;;   :diminish
;;   :config
;;   (add-hook 'prog-mode-hook #'highlight-indent-guides-mode)
;;   (setq highlight-indent-guides-method 'character)
;;   (setq highlight-indent-guides-character 9615) ; left-align vertical bar
;;   (setq highlight-indent-guides-auto-character-face-perc 20))

;; (use-package find-file-in-project
;;   :config
;;   (setq ffip-use-rust-fd t))

(use-package k8s-mode
  :hook (k8s-mode . yas-minor-mode))

(use-package protobuf-mode)

;; ==============
;;    EFUNS
;; ==============

(defun load-local (file)
  (load (f-expand file user-emacs-directory)))

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


;; === ============ ADD THEME FOLDER =========================
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
