;; Reduce the frequency of garbage collection by making it happen on
;; each 50MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold 50000000
      gc-cons-percentage 0.6)

;; Automatically reload files when they change on disk
(global-auto-revert-mode 1)
(setq auto-revert-verbose nil)

;; Bootstrap configuration for straight.el
(setq straight-use-package-by-default t)
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

;; Move custom configuration variables set by Emacs, to a seperate file
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)


;; ==========
;; APPEARANCE
;; ==========

;; Font
(set-face-attribute 'default nil
                    :family "Fira Code"
                    :height 150
                    :weight 'normal
                    :width 'normal)

;; Type
(setq-default line-spacing 0.3
              help-window-select t
              ;; Hide cursor in inactive windows
              cursor-in-non-selected-windows nil
              truncate-lines t)


;; Workaround for alt not working as meta key
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

;; Cursor style
(set-default 'cursor-type '(bar . 2)) ;; or (set-default 'cursor-type 'box)

;; Toggle cursor blinking at rest
(blink-cursor-mode 1)

;; TODO
(global-hl-line-mode nil)

;; TODO
(set-face-attribute hl-line-face nil :underline nil)

;; Rewrite selected text
(delete-selection-mode 1)

;; don't show the tool bar
(tool-bar-mode -1)

;; don't show the scroll bar
(scroll-bar-mode -1)

;; Make the title bar blend with the background color
;; Set the appearance to light/dark depending on your theme
(add-to-list 'default-frame-alist
             '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist
             '(ns-appearance . dark))

;; Show full file path in the title bar
(setq
 frame-title-format
 '((:eval (if (buffer-file-name)
              (abbreviate-file-name (buffer-file-name))
            "%b"))))

;; Line numbers
;; Add some padding when displaying line numbers
(setq linum-format "%5d ")
(add-hook 'prog-mode-hook 'display-line-numbers-mode)


;; =============
;; SANE DEFAULTS
;; =============

;; Use y/n instead of full yes/no for confirmation messages
(fset 'yes-or-no-p 'y-or-n-p)

;; Use spaces instead of tabs
(setq tab-width 2)
(setq js-indent-level 2)
(setq css-indent-offset 2)
(setq c-basic-offset 2)
(setq-default indent-tabs-mode nil)
(setq-default c-basic-offset 2)
(setq-default tab-width 2)
(setq-default c-basic-indent 2)

;; Type over selected text
(delete-selection-mode 1)

;; Kill whole line
(global-set-key (kbd "s-<backspace>") 'kill-whole-line)

;; Use Cmd for movement
(global-set-key (kbd "s-<right>") (kbd "C-e"))  ;; End of line
(global-set-key (kbd "s-<left>") (kbd "C-a"))   ;; Beginning of line

;; Kills the current buffer without displaying the annoying menu.
;; A confirmation will be asked for, if the buffer has been modified
(global-set-key (kbd "C-x k") 'kill-this-buffer)

;; Remove trailing whitespace before saving
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; On OS X, an Emacs instance started from the graphical user
;; interface will have a different environment than a shell in a
;; terminal window, because OS X does not run a shell during the
;; login. Obviously this will lead to unexpected results when
;; calling external utilities like make from Emacs.
;; This library works around this problem by copying important
;; environment variables from the user's shell.
;; https://github.com/purcell/exec-path-from-shell
(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

;; M-x with recently/frequently used commands
(use-package smex
  :config
  (global-set-key (kbd "M-x") 'smex)
  (global-set-key (kbd "s-x") 'smex))

;; Install and setup company-mode for autocompletion
(use-package company
  :bind (("C-p" . company-select-previous)
         ("C-n" . company-select-next))
  :init
  (add-hook 'prog-mode-hook 'company-mode)
  :config
  (global-company-mode)
  (setq company-tooltip-limit 10)
  (setq company-idle-delay 0.2)
  (setq company-echo-delay 0)
  (setq company-minimum-prefix-length 2)
  (setq company-require-match nil)
  (setq company-selection-wrap-around t)
  (setq company-tooltip-align-annotations t)
  (setq company-tooltip-flip-when-above nil)
  (setq company-dabbrev-ignore-case nil
        company-dabbrev-downcase nil)
  ;; weight by frequency
  (setq company-transformers '(company-sort-by-occurrence)))

;; Better syntax highlighting
(use-package clojure-mode-extra-font-locking)

;; Highlight matching parentheses
(require 'paren)
(show-paren-mode 1)
(setq show-paren-delay 1)
(set-face-background 'show-paren-match (face-background 'default))
(if (eq (frame-parameter nil 'background-mode) 'dark)
    (set-face-foreground 'show-paren-match "red")
  (set-face-foreground 'show-paren-match "black"))
(set-face-attribute 'show-paren-match nil :weight 'extra-bold)

;; Add ability to shift between buffers using shift+arrow keys.
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

;; Paredit makes it easier to navigate/edit s-expressions as blocks.
(use-package paredit
  :init
  (add-hook 'clojure-mode-hook 'enable-paredit-mode)
  (add-hook 'cider-repl-mode-hook 'enable-paredit-mode)
  (add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode))

;; To add some colors to those boring parens
(use-package rainbow-delimiters
  :init (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(define-clojure-indent
  (defrecord 1)
  (as-> 2))

;; Cider integrates a Clojure buffer with a REPL
(use-package cider
  :init
  (setq cider-repl-pop-to-buffer-on-connect t
        cider-show-error-buffer t
        cider-auto-select-error-buffer t
        cider-repl-history-file "~/.emacs.d/cider-history"
        cider-repl-wrap-history t
        cider-repl-history-size 100
        cider-repl-use-clojure-font-lock t
        cider-docview-fill-column 70
        cider-stacktrace-fill-column 76
        nrepl-popup-stacktraces nil
        nrepl-log-messages nil
        nrepl-hide-special-buffers t
        cider-repl-use-pretty-printing t
        cider-repl-result-prefix ";; => "
        cider-repl-display-help-banner nil
        cider-font-lock-dynamically '(macro core function var))

  :config
  (add-hook 'cider-mode-hook #'eldoc-mode)
  (add-hook 'cider-repl-mode-hook #'paredit-mode)
  (add-hook 'cider-repl-mode-hook #'company-mode)
  (add-hook 'cider-repl-mode-hook
            (lambda ()
              (local-set-key (kbd "C-l") 'cider-repl-clear-buffer)))
  (add-hook 'cider-mode-hook #'company-mode)
  (add-hook 'cider-mode-hook
            (lambda ()
              (local-set-key (kbd "<C-return>") 'cider-eval-last-sexp)
              (local-set-key (kbd "C-c C-n") 'cider-eval-buffer)
              (local-set-key (kbd "C-x C-i") 'cider-inspect-last-sexp))))

;; Adds some niceties/refactoring support
(use-package clj-refactor
  :config
  (setq cljr-warn-on-eval nil)
  (add-hook 'clojure-mode-hook
            (lambda ()
              (clj-refactor-mode 1))))

(use-package flycheck-clj-kondo
  :config
  (remove-hook 'clojure-mode-hook
               (lambda ()
                 (require 'flycheck-clj-kondo))))

;; Aggressively indents your clojure code
(use-package aggressive-indent
  :commands (aggressive-indent-mode)
  :config
  (add-hook 'clojure-mode-hook 'aggressive-indent-mode))

;; Operate (list, search, replace....) on files at a project level.
(use-package projectile
  :bind (:map projectile-mode-map
              ("s-p" . projectile-command-map))
  :init
  (setq-default projectile-cache-file
                (expand-file-name ".projectile-cache" user-emacs-directory))
  (add-hook 'prog-mode-hook #'projectile-mode)

  :config
  (projectile-mode)
  (setq projectile-completion-system 'ivy)
  (setq-default projectile-enable-caching t
                projectile-mode-line-prefix ""
                projectile-sort-order 'recentf
                ;; Show project (if any) name in modeline
                projectile-mode-line '(:eval (projectile-project-name))))

;; TODO
(use-package ido-vertical-mode
  :config
  (ido-vertical-mode 1)
  (setq ido-use-faces t
        ido-vertical-show-count t
        ido-vertical-define-keys 'C-n-C-p-up-down-left-right))

;; TODO
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

;; Apply grep changes to file without sed etc.
(use-package wgrep)

;; Replace grep/ag with ripgrep
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

;; Magit: The only git interface you'll ever need
(use-package magit
  :bind ("C-x g" . 'magit-status)
  :config
  (setq magit-set-upstream-on-push 'askifnotset))


;; Org mode
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

(setq org-todo-keywords
      '((sequence "TODO" "DOING" "|" "DONE")))

(use-package org-bullets
  :after org
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(use-package all-the-icons)

(use-package multiple-cursors
  :config
  (setq-default mc/edit-lines-empty-lines 'ignore
                mc/insert-numbers-default 1)
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this))

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

(use-package hl-todo
  :config (global-hl-todo-mode t))

(use-package counsel-projectile
  :config (counsel-projectile-mode))

(use-package fast-scroll
  :config
  (fast-scroll-config)
  (fast-scroll-mode 1))

(use-package golden-ratio
  :diminish golden-ratio-mode
  :init
  (golden-ratio-mode 1))

(use-package dired-sidebar
  :bind (("C-x C-n" . dired-sidebar-toggle-sidebar))
  :commands (dired-sidebar-toggle-sidebar)
  :init
  (add-hook 'dired-sidebar-mode-hook
            (lambda ()
              (unless (file-remote-p default-directory)
                (auto-revert-mode))))
  :config
  (push 'toggle-window-split dired-sidebar-toggle-hidden-commands)
  (push 'rotate-windows dired-sidebar-toggle-hidden-commands)
  (setq dired-sidebar-subtree-line-prefix "__")
  (setq dired-sidebar-face '(:family "Menlo"))
  (setq dired-sidebar-use-custom-font t))


;; ===================
;; User customizations
;; ===================

;; Add your customizations to `init-user.el`
(when (file-exists-p "~/.emacs.d/init-user.el")
  (setq user-custom-file "~/.emacs.d/init-user.el")
  (load user-custom-file)
  (put 'downcase-region 'disabled nil))
(put 'narrow-to-region 'disabled nil)

;; Reset GC to reasonable defaults
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold 16777216
                  gc-cons-percentage 0.1)))
