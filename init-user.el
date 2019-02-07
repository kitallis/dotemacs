(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)

(set-face-attribute 'default nil
                    :family "Iosevka Term"
                    :height 150
                    :weight 'normal
                    :width 'normal)

(setq-default line-spacing 0.1
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
      buffer-file-coding-system 'utf-8)

;; Rewrite selected text
(delete-selection-mode 1)

;; ============= THIRD PARTY PACKAGES ================

(use-package oceanic-theme
  :ensure t
  :config
  (load-theme 'oceanic))

(use-package expand-region
  :ensure t
  :config (global-set-key (kbd "C-;") 'er/expand-region))

(use-package ido-vertical-mode
  :ensure t
  :config (ido-vertical-mode 1))

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
  :init (use-package rg :ensure t)
  :bind (("s-g" . 'counsel-rg)
         ("C-x C-r" . 'counsel-recentf))
  :config
  (global-set-key (kbd "s-g") 'counsel-rg)
  (setq recentf-max-saved-items 50)
  (setq recentf-auto-cleanup (* 24 60 60))
  (global-set-key (kbd "C-x C-r") 'counsel-recentf))

(use-package org
  :ensure t
  :config
  (setq org-directory "~/Box Sync/org-notes"
        org-default-notes-file (concat org-directory "/notes.txt")
        org-export-coding-system 'utf-8
        org-ellipsis " ▼ "
        org-startup-indented t))

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
  (setq doom-modeline-buffer-file-name-style 'truncate-upto-project
        doom-modeline-icon t
        doom-modeline-major-mode-icon t
        doom-modeline-minor-modes nil
        doom-modeline-github nil
        doom-modeline-version nil
        doom-modeline-height 10))

(use-package undo-tree
  :ensure t
  :bind ("s-Z" . 'undo-tree-redo)
  :config
  (global-undo-tree-mode))

(use-package zoom-window
  :ensure t
  :config
  (setq zoom-window-mode-line-color "green"))

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "pandoc"))

(use-package smartparens
  :ensure t
  :init
  (add-hook 'markdown-mode-hook #'smartparens-mode))

(use-package hl-todo
  :ensure t
  :config
  (global-hl-todo-mode t))

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
  (fringe-mode 10)
  (kg/set-fringe-background)
  (setq linum-format "%5d "))

;; ================ GLOBAL KEYBINDINGS ++++++++++++++++++++

(global-set-key (kbd "s-l") 'goto-line)
(global-set-key (kbd "s-t") 'projectile-find-file)
(global-set-key (kbd "M-j") (lambda () (interactive) (join-line -1)))
(global-set-key (kbd "s-{") 'bs-cycle-previous)
(global-set-key (kbd "s-}") 'bs-cycle-next)
(global-set-key (kbd "C-x 3") 'kg/split-right-and-move)
(global-set-key (kbd "C-x 2") 'kg/split-below-and-move)
(global-set-key (kbd "C-a") 'kg/beginning-of-line-dwim)
(global-set-key [(meta shift down)] 'kg/duplicate-start-of-line-or-region)
