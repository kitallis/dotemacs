;; A minimial setup for Clojurians

(require 'package)

;; Define package repositories
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))


;; Load and activate emacs packages. Do this first so that the
;; packages are loaded before you start trying to modify them.
;; This also sets the load path.
(package-initialize)


;; Reduce the frequency of garbage collection by making it happen on
;; each 50MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold 50000000)


;; Automatically reload files when they change on disk
(global-auto-revert-mode 1)
(setq auto-revert-verbose nil)


;; Install use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-verbose t)
(setq-default use-package-always-ensure t)


;; Show full file path in the title bar
(setq
 frame-title-format
 '((:eval (if (buffer-file-name)
              (abbreviate-file-name (buffer-file-name))
            "%b"))))


;; Use y/n instead of full yes/no for confirmation messages
(fset 'yes-or-no-p 'y-or-n-p)


;; Line numbers
(global-linum-mode t)


;; Remove trailing whitespace before saving
(add-hook 'before-save-hook 'delete-trailing-whitespace)


;; M-x with recently/frequently used commands
(use-package smex)
(global-set-key (kbd "M-x") 'smex)


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
  ;; weight by frequency
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
        cider-show-error-buffer t
        cider-auto-select-error-buffer t
        cider-repl-history-file "~/.emacs.d/cider-history"
        cider-repl-wrap-history t
        cider-repl-history-size 100
        cider-repl-use-clojure-font-lock t
        cider-docview-fill-column 70
        cider-stacktrace-fill-column 76
        ;; Stop error buffer from popping up while working in buffers other than REPL:
        nrepl-popup-stacktraces nil
        nrepl-log-messages nil
        nrepl-hide-special-buffers t
        cider-repl-use-pretty-printing t
        cider-repl-result-prefix ";; => ")

  :config
  (add-hook 'cider-mode-hook #'eldoc-mode)
  (add-hook 'cider-repl-mode-hook #'paredit-mode)
  (add-hook 'cider-repl-mode-hook #'company-mode)
  (add-hook 'cider-mode-hook #'company-mode)
  (add-hook 'cider-mode-hook
            (local-set-key (kbd "<C-return>") 'cider-eval-defun-at-point)))


;; Adds some niceties/refactoring support
(use-package clj-refactor
  :ensure t
  :config
  (add-hook 'clojure-mode-hook
            (lambda ()
              (clj-refactor-mode 1))))


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


;; Magit: The only git interface you'll ever need
(use-package magit :ensure t)

;; User customizations
(when (file-exists-p "~/.emacs.d/init-user.el")
  (setq user-custom-file "~/.emacs.d/init-user.el")
  (load user-custom-file))
