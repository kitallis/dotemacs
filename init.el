(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(defconst *is-a-mac* (eq system-type 'darwin))

;; Reduce the frequency of garbage collection by making it happen on
;; each 50MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold 50000000 gc-cons-percentage 0.6)

;; Allow loading custom config from specified dir
(add-to-list 'load-path (expand-file-name "conf" user-emacs-directory))

;; Bootstrap configuration for straight.el
(setq straight-use-package-by-default t)
(defvar bootstrap-version)
(let ((bootstrap-file (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
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

;; Load custom configurations
;; These required in a "valid" order
(require 'init-efuns)
(require 'init-sane-defaults)
(require 'init-appearance)
(require 'init-editing)
(require 'init-lsp)
(require 'init-code)
(require 'init-clojure)
(require 'init-completions)
(require 'init-dired)
(require 'init-git)
(require 'init-org)
(require 'init-projectile)
(require 'init-keybinds)

;; User customizations
;; Add your customizations / overrides to *user-customizations-path*
(defconst *user-customizations-path* "~/.emacs.d/init-user.el")
(when (file-exists-p *user-customizations-path*)
  (setq user-custom-file *user-customizations-path*)
  (load user-custom-file)
  (put 'downcase-region 'disabled nil))
(put 'narrow-to-region 'disabled nil)

;; Reset GC to reasonable defaults
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold 16777216
                  gc-cons-percentage 0.1)))
