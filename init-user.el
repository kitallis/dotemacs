(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)

(set-face-attribute 'default nil
                    :family "Fira Code"
                    :height 150
                    :weight 'normal
                    :width 'normal)

(setq-default line-spacing 0.3
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

;;Treat hyphens as a word character when transposing words
(defvar clojure-mode-with-hyphens-as-word-sep-syntax-table
  (let ((st (make-syntax-table clojure-mode-syntax-table)))
    (modify-syntax-entry ?- "w" st)
    st))

(defun transpose-words-with-hyphens (arg)
  "Treat hyphens as a word character when transposing words"
  (interactive "*p")
  (with-syntax-table clojure-mode-with-hyphens-as-word-sep-syntax-table
    (transpose-words arg)))

(define-key clojure-mode-map (kbd "M-t") 'transpose-words-with-hyphens)

;; ======
;; THEMES
;; ======

(use-package monokai-pro-theme)
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
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (load-theme 'doom-challenger-deep))
(use-package abyss-theme :disabled t)
(use-package gruvbox-theme :disabled t)
(use-package github-theme :disabled t)
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
  (define-key ivy-minibuffer-map (kbd "C-m") 'ivy-alt-done))

(use-package wgrep)

(use-package rg
  :config
  (setq rg-command-line-flags '("-w"))
  (setq rg-ignore-case 'smart)

  (rg-define-search kg/grep-vc-or-dir
    :query ask
    :format regexp
    :files "everything"
    :dir (let ((vc (vc-root-dir)))
           (if vc
               vc                         ; search root project dir
             default-directory))          ; or from the current dir
    :confirm prefix
    :flags ("--hidden -g !.git")))

(use-package counsel
  :after rg
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
  (setq org-directory "~/org-notes"
        org-default-notes-file (concat org-directory "/notes.txt")
        org-export-coding-system 'utf-8
        org-ellipsis " ▼ "
        org-startup-indented nil
        org-src-fontify-natively t
        org-src-preserve-indentation t)
  (setq org-capture-templates
        '(("l" "Add a link to the linklog" entry
           (file+olp+datetree (lambda () (concat org-directory "/linklog.org")))
           "**** %?")))
  (add-hook 'org-mode-hook
            (lambda () (when (fboundp 'org-mac-grab-link)
                         (load-file "~/.emacs.d/org-mac-link.el")))))

(use-package org-bullets
  :after org
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(use-package multiple-cursors
  :config
  (setq-default mc/edit-lines-empty-lines 'ignore
                mc/insert-numbers-default 1)
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this))

(use-package all-the-icons
  :disabled t)

(use-package doom-modeline
  :hook (after-init . doom-modeline-init)
  :init
  (set-face-attribute 'mode-line nil :height 140)
  (set-face-attribute 'mode-line-inactive nil :height 140)
  (set-face-attribute 'mode-line nil :family "Inconsolata Nerd Font" :height 140)
  :config
  (setq doom-modeline-buffer-file-name-style 'relative-from-project
        doom-modeline-icon (display-graphic-p)
        doom-modeline-major-mode-color-icon t
        doom-modeline-major-mode-icon t
        doom-modeline-minor-modes nil
        doom-modeline-github nil
        doom-modeline-version nil
        doom-modeline-height 10
        doom-modeline-bar-width 1
        doom-modeline-buffer-encoding nil
        doom-modeline-vcs-max-length 50
        doom-modeline-gnus nil
        doom-modeline-irc nil
        doom-modeline-persp-name nil
        doom-modeline-window-width-limit fill-column))

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

(use-package elixir-mode)

(use-package rust-mode
  :config
  (add-hook 'rust-mode-hook #'flycheck-mode)
  (add-hook 'rust-mode-hook #'smartparens-mode)
  (add-hook 'rust-mode-hook
            (lambda ()
              (local-set-key (kbd "C-c SPC") #'rust-format-buffer)
              (local-set-key (kbd "C-c c") #'rust-compile)
              (local-set-key (kbd "C-c C-t") #'rust-test)
              (local-set-key (kbd "C-c C-r") #'rust-run)
              (setq company-minimum-prefix-length 1)))

  (use-package racer
    :init
    (setq racer-rust-src-path
          "/Users/kiran/.rustup/toolchains/stable-x86_64-apple-darwin/lib/rustlib/src/rust/src")
    :config
    (add-hook 'rust-mode-hook #'racer-mode)
    (add-hook 'racer-mode-hook #'eldoc-mode)
    (add-hook 'racer-mode-hook #'company-mode)
    (define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)
    (define-key rust-mode-map (kbd "C-c C-d") #'racer-describe)
    (setq company-tooltip-align-annotations t
          racer-eldoc-timeout 0.5))

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

(use-package web-mode
  :mode (("\\.html?\\'" . web-mode)
         ("\\.css\\'"   . web-mode)
         ("\\.json\\'"  . web-mode))
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-css-indent-offset 2))

(use-package k8s-mode
  :hook (k8s-mode . yas-minor-mode))

(use-package dockerfile-mode
  :config
  (require 'dockerfile-mode)
  (add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode)))

(use-package golden-ratio
  :diminish golden-ratio-mode
  :init
  (golden-ratio-mode 1))

(use-package dired-sidebar
  :commands (dired-sidebar-toggle-sidebar)
  :config
  (setq dired-sidebar-theme 'icons)
  (setq dired-sidebar-use-custom-font t)
  (setq dired-sidebar-face '(:family "Helvetica" :height 110)))

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
(global-set-key (kbd "s-w") 'kill-this-buffer)
(global-set-key (kbd "<s-S-return>") 'kg/toggle-maximize-buffer)


;; === ============ ADD THEME FOLDER =========================
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
