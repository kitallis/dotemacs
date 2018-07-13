;; A minimial setup for Clojurians

(require 'package)

;; Define package repositories
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/"))

;; Load and activate emacs packages. Do this first so that the
;; packages are loaded before you start trying to modify them.
;; This also sets the load path.
(package-initialize)
(setq package-enable-at-startup nil)


;; Automatically reload files when they change on disk
(global-auto-revert-mode 1)
(setq auto-revert-verbose nil)


;; Install use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
 (require 'use-package)
(setq use-package-verbose t)


;; On OS X, an Emacs instance started from the graphical user
;; interface will have a different environment than a shell in a
;; terminal window, because OS X does not run a shell during the
;; login. Obviously this will lead to unexpected results when
;; calling external utilities like make from Emacs.
;; This library works around this problem by copying important
;; environment variables from the user's shell.
;; https://github.com/purcell/exec-path-from-shell
(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))


;; Move custom configuration variables set by Emacs, to a seperate file
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)


;; Install and setup company-mode for autocompletion
(use-package company
  :ensure t
  :init
  (add-hook 'prog-mode-hook 'company-mode)
  :config
  (global-company-mode)
  (setq company-tooltip-limit 10)
  (setq company-idle-delay 0.2)
  (setq company-echo-delay 0)
  (setq company-minimum-prefix-length 3)
  (setq company-require-match nil)
  (setq company-selection-wrap-around t)
  (setq company-tooltip-align-annotations t)
  (setq company-tooltip-flip-when-above t)
   ; weight by frequency
  (setq company-transformers '(company-sort-by-occurrence)))


;; Better syntax highlighting
(use-package clojure-mode-extra-font-locking
  :ensure t)


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
  :ensure t
  :init
  (add-hook 'clojure-mode-hook 'enable-paredit-mode)
  (add-hook 'cider-repl-mode-hook 'enable-paredit-mode))


;; To add some colors to those boring parens
(use-package rainbow-delimiters
  :ensure t
  :init (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))


;; Cider integrates a Clojure buffer with a REPL
(use-package cider
  :ensure t
  :init

  (setq cider-repl-pop-to-buffer-on-connect t
        cider-mode-line nil
        cider-prompt-for-symbol nil
        cider-show-error-buffer t
        cider-auto-select-error-buffer t
        cider-repl-history-file "~/.emacs.d/cider-history"
        cider-repl-wrap-history t
        cider-repl-history-size 100
        cider-repl-use-clojure-font-lock t
        cider-docview-fill-column 70
        cider-stacktrace-fill-column 76
        nrepl-hide-special-buffers t
        nrepl-popup-stacktraces nil
        nrepl-log-messages nil
        nrepl-hide-special-buffers t
        cider-repl-use-pretty-printing t
        cider-repl-display-help-banner nil
        cider-repl-result-prefix ";; => "
        cider-show-error-buffer nil)

  :config

  (add-hook 'cider-mode-hook #'eldoc-mode)
  (add-hook 'cider-repl-mode-hook #'paredit-mode)
  (add-hook 'cider-repl-mode-hook #'company-mode)
  (add-hook 'cider-mode-hook #'company-mode)
  (add-hook 'cider-mode-hook
            (local-set-key (kbd "<C-return>") 'cider-eval-defun-at-point)))


;; Aggressively indents your clojure code
(use-package aggressive-indent
  :ensure t
  :commands (aggressive-indent-mode)
  :config
  (add-hook 'clojure-mode-hook 'aggressive-indent-mode))


;; Operate (list, search, replace....) on files at a project level.
(use-package projectile
  :ensure t
  :init
  (setq-default projectile-cache-file
		(expand-file-name ".projectile-cache" user-emacs-directory))

  (add-hook 'prog-mode-hook #'projectile-mode)

  :config
  (projectile-mode)
  (setq-default projectile-enable-caching t
                ;; Show project (if any) name in modeline
                projectile-mode-line '(:eval (projectile-project-name))))


;; Enable clj-refactor mode
(use-package clj-refactor
  :ensure t
  :init

  :config
  (add-hook 'clojure-mode-hook (lambda ()
				 (clj-refactor-mode 1)
				 (yas-minor-mode 1)
				 (cljr-add-keybindings-with-prefix "C-c C-x")))
  (setq cljr-favor-prefix-notation nil)
  (setq cljr-warn-on-eval nil)
  (setq cljr-find-usages-ignore-analyzer-errors t)
  (setq cljr-ignore-analyzer-errors t))


;; Magit: The only git interface you'll ever need
(use-package magit :ensure t)


;; User customizations
(when (file-exists-p "~/.emacs.d/init-user.el")
  (setq user-custom-file "~/.emacs.d/init-user.el")
  (load user-custom-file))


(set-variable 'cider-stacktrace-frames-background-color "#161616")
(set-variable 'cider-test-items-background-color "#333333")

(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;; Add greatest ever titlebar
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))


;; Enable neotreee - fancy file browser
(use-package neotree
  :init
  (use-package all-the-icons)

  :config
  (global-set-key (kbd "<f5>") 'neotree-toggle)

  (setq-default neo-smart-open t)
  (setq neo-theme (if (display-graphic-p) 'icons 'nerd))
  (setq neo-show-hidden-files t)
  ;; Scale the text down a notch when in a neotree buffer
  (defun kg/text-scale-down ()
    (interactive)
    (progn
      (text-scale-adjust 0)
      (text-scale-decrease 0)))

  (add-hook 'neo-after-create-hook
            (lambda (_)
              (call-interactively #'kg/text-scale-down))))


;; Enable spaceline - fancy modeline
(use-package spaceline
  :ensure t
  :init
  (require 'spaceline-config)
  (let ((faces '(mode-line
		 mode-line-buffer-id
		 mode-line-emphasis
		 mode-line-highlight
		 mode-line-inactive)))
    (mapc
     (lambda (face) (set-face-attribute face nil :font "Menlo:weight=light:pixelsize=11"))
     faces))
  ;; solves the issue with incorrect seperator colors in modeline
  ;; powerline now takes sRGB colorspace for rendering
  (setq powerline-image-apple-rgb t)
  :config
  (spaceline-spacemacs-theme)
  (setq spaceline-workspace-numbers-unicode nil)
  (setq spaceline-window-numbers-unicode nil)
  (setq spaceline-org-clock-p nil)
  (setq spaceline-highlight-face-func 'spaceline-highlight-face-modified))
