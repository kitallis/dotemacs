(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)

(set-face-attribute 'default nil
                    :family "Office Code Pro"
                    :height 130
                    :weight 'normal
                    :width 'normal)

(setq-default line-spacing 0.2
              help-window-select t)

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

(set-default 'cursor-type t)

(global-hl-line-mode nil)
(set-face-attribute hl-line-face nil :underline t)

;; Rewrite selected text
(delete-selection-mode 1)

(setq org-todo-keywords
      '((sequence "TODO" "DOING" "|" "DONE")))

;; ======
;; THEMES
;; ======

(use-package monokai-pro-theme)
(use-package nimbus-theme :disabled t)
(use-package srcery-theme :disabled t)
(use-package nord-theme :disabled t)
(use-package darkokai-theme
  :disabled t
  :config
  (setq darkokai-mode-line-padding 1)
  (load-theme 'darkokai))
(use-package doom-themes
  :disabled t
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (load-theme 'doom-challenger-deep))

;; ==========================
;; OTHER THIRD PARTY PACKAGES
;; ==========================

(use-package expand-region
  :ensure t
  :config (global-set-key (kbd "C-;") 'er/expand-region))

(use-package ido-vertical-mode
  :ensure t
  :config
  (ido-vertical-mode 1)
  (setq ido-use-faces t
        ido-vertical-show-count t
        ido-vertical-define-keys 'C-n-and-C-p-only))

(use-package ivy
  :ensure t
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
  (use-package flx
    :ensure t
    :init
    (setq ivy-re-builders-alist '((t . ivy--regex-fuzzy)))))

(use-package counsel
  :ensure t
  :init (use-package rg
          :ensure t
          :config
          (setq rg-command-line-flags '("-w"))
          (setq rg-ignore-case 'smart))
  :config
  (global-set-key (kbd "s-g") 'counsel-rg)
  (global-set-key (kbd "C-x C-r") 'counsel-recentf)
  (setq counsel-rg-base-command "rg -i -w --no-heading --line-number %s .")
  (setq recentf-max-saved-items 50)
  (setq recentf-auto-cleanup (* 24 60 60)))

(use-package swiper
  :ensure t
  :config
  (global-set-key (kbd "s-f") 'swiper))

(use-package org
  :ensure t
  :config
  (setq org-directory "~/Box Sync/org-notes"
        org-default-notes-file (concat org-directory "/notes.txt")
        org-export-coding-system 'utf-8
        org-ellipsis " ▼ "
        org-startup-indented t
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
                         (load-file "~/.emacs.d/org-mac-link.el"))))
  (use-package org-journal
    :ensure t
    :bind (("C-c y" . org-journal-previous-entry))
    :config
    (global-set-key (kbd "C-c t") (lambda () (interactive) (org-journal-new-entry t)))
    :custom
    (org-journal-find-file #'find-file)
    (org-journal-dir "~/Box Sync/Kiran/journal")
    (org-journal-file-format "%Y-%m-%d.org")
    ;; (org-journal-date-format "#+TITLE: Journal Entry: %e %b %Y (%A)")
    (org-journal-date-format "%A, %d %B %Y")))

(use-package multiple-cursors
  :ensure t
  :config
  (setq-default mc/edit-lines-empty-lines 'ignore
                mc/insert-numbers-default 1)
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this))

(use-package doom-modeline
  :ensure t
  :pin melpa-stable
  :hook (after-init . doom-modeline-init)
  :config
  (setq doom-modeline-buffer-file-name-style 'file-name
        doom-modeline-icon nil
        doom-modeline-major-mode-icon nil
        doom-modeline-minor-modes nil
        doom-modeline-github nil
        doom-modeline-version nil
        doom-modeline-height 10
        doom-modeline-bar-width 3)
  (set-face-attribute 'mode-line nil :height 130)
  (set-face-attribute 'mode-line-inactive nil :height 130))

(use-package undo-tree
  :ensure t
  :bind ("s-Z" . 'undo-tree-redo)
  :config
  (global-undo-tree-mode))

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "pandoc")
  :config
  (add-hook 'gfm-mode-hook 'linum-mode)
  (add-hook 'markdown-mode-hook 'linum-mode))

(use-package smartparens
  :ensure t
  :init
  (add-hook 'markdown-mode-hook 'smartparens-mode))

(use-package hl-todo
  :ensure t
  :config
  (global-hl-todo-mode t))

(use-package yaml-mode :ensure t)
(use-package json-mode :ensure t)

(use-package counsel-projectile
  :ensure t
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

(use-package go-mode)

(use-package ivy-posframe
  :disabled t
  :config
  (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-window-center)))
  (ivy-posframe-mode 1))

(use-package fast-scroll
  :pin melpa-stable
  :config
  (fast-scroll-config)
  (fast-scroll-mode 1))

;; =============== EFUNS ==================

(defun kg/split-below-and-move ()
  (interactive)
  (split-window-below)
  (other-window 1))

(defun kg/split-right-and-move ()
  (interactive)
  (split-window-right)
  (other-window 1))

(defun kg/beginning-of-line-dwim ()
  "Toggle between moving point to the first non-whitespace character, and the start of the line."
  (interactive)
  (let ((start-position (point)))
    ;; Move to the first non-whitespace character.
    (back-to-indentation)

    ;; If we haven't moved position, go to start of the line.
    (when (= (point) start-position)
      (move-beginning-of-line nil))))

(defun kg/duplicate-start-of-line-or-region ()
  "Duplicate start of line or region."
  (interactive)
  (if mark-active
      (kg/duplicate-region)
    (kg/duplicate-start-of-line)))

(defun kg/duplicate-start-of-line ()
  "Duplicate start of line."
  (let ((text (buffer-substring (point)
                                (beginning-of-thing 'line))))
    (forward-line)
    (push-mark)
    (insert text)
    (open-line 1)))

(defun kg/duplicate-region ()
  "Duplicate start of region."
  (let* ((end (region-end))
         (text (buffer-substring (region-beginning)
                                 end)))
    (goto-char end)
    (insert text)
    (push-mark end)
    (setq deactivate-mark nil)
    (exchange-point-and-mark)))

(defun kg/set-fringe-background ()
  "Set the fringe background to the same color as the regular background."
  (interactive)
  (custom-set-faces
   `(fringe ((t (:background ,(face-background 'default)))))))

(add-hook 'after-init-hook #'kg/set-fringe-background)

(defun kg/reset-ui ()
  "Reset some UI components after changing a theme."
  (interactive)
  ;; (fringe-mode 10)
  (fringe-mode '(0 . 0))
  (kg/set-fringe-background)
  (setq linum-format "%5d "))

(defun kg/delete-this-buffer-and-file ()
  "Remove file connected to current buffer and kill buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (when (yes-or-no-p "Are you sure you want to remove this file? ")
        (delete-file filename)
        (kill-buffer buffer)
        (message "File '%s' successfully removed" filename)))))

(defun kg/search-marked-region-if-available (start end)
  "Pre-fill counsel-rg with marked region if available."
    (interactive "r")
    (if (use-region-p)
        (let ((regionp (buffer-substring start end)))
          (counsel-rg regionp))
      (counsel-rg)))

;; ================ GLOBAL KEYBINDINGS ++++++++++++++++++++

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


;; === ============ ADD THEME FOLDER =========================
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
;; (load-theme 'molokai)
