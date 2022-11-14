;; A utility package to collect various Icon Fonts and propertize them within Emacs.
(use-package all-the-icons)

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
(setq linum-format "%4d ")
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (load-theme 'doom-Iosvkem))

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

(provide 'init-appearance)
