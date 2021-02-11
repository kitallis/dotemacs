;; A utility package to collect various Icon Fonts and propertize them within Emacs.
;; (use-package all-the-icons)

;; Automatic resizing of Emacs windows to the golden ratio
(use-package golden-ratio
  :diminish golden-ratio-mode
  :init
  (golden-ratio-mode 1))

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

;; Font settings
(set-face-attribute 'default nil
                    :family "Fira Code"
                    :height 150
                    :weight 'normal
                    :width 'normal)

;; Type settings
(setq-default line-spacing 0.3
              help-window-select t
              ;; Hide cursor in inactive windows
              cursor-in-non-selected-windows nil
              truncate-lines t)

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

;; Edit with multiple cursors at once
(use-package multiple-cursors
  :config
  (setq-default mc/edit-lines-empty-lines 'ignore
                mc/insert-numbers-default 1)
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this))

(provide 'init-appearance)
