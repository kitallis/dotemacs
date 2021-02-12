;; Edit with multiple cursors at once
(use-package multiple-cursors
  :config
  (setq-default mc/edit-lines-empty-lines 'ignore
                mc/insert-numbers-default 1)
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this))

;; Growing region select
(use-package expand-region
  :config (global-set-key (kbd "C-;") 'er/expand-region))

;; Treat undo history as branching tree
(use-package undo-tree
  :bind ("s-Z" . 'undo-tree-redo)
  :config
  (global-undo-tree-mode))

(setq save-interprogram-paste-before-kill t)

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

;; ensure scrolling remains fast
(use-package fast-scroll
  :config
  (fast-scroll-config)
  (fast-scroll-mode 1))

;; highlight TODO everywhere
(use-package hl-todo
  :config (global-hl-todo-mode t))

;; Distraction-free mode
(use-package writeroom-mode
  :config
  (setq writeroom-mode-toggle-mode-line t))

(provide 'init-editing)
