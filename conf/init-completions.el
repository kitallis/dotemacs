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

;; M-x with recently/frequently used commands
(use-package smex
  :config
  (global-set-key (kbd "M-x") 'smex)
  (global-set-key (kbd "s-x") 'smex))

(use-package swiper
  :config
  (global-set-key (kbd "s-f") 'swiper))

(use-package counsel
  :after rg
  :config
  ;; (global-set-key (kbd "s-g") 'counsel-rg)
  (global-set-key (kbd "C-x C-r") 'counsel-recentf)
  (setq counsel-rg-base-command "rg -i -w --no-heading --line-number %s .")
  (setq recentf-max-saved-items 50)
  (setq recentf-auto-cleanup (* 24 60 60)))

(provide 'init-completions)
