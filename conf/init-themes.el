(use-package monokai-pro-theme
  :disabled t)

(use-package moe-theme
  :disabled t
  :config
  (setq moe-theme-highlight-buffer-id t
        show-paren-style 'parenthesis)
  (moe-theme-set-color 'blue)
  (moe-dark))

(use-package srcery-theme
  :disabled t)

(use-package nord-theme
  :disabled t)

(use-package darkokai-theme
  :config
  (setq darkokai-mode-line-padding 1))

(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (load-theme 'doom-challenger-deep))

(use-package abyss-theme
  :disabled t)

(use-package gruvbox-theme
  :disabled t)

(use-package dracula-theme
  :disabled t
  :config
  (load-theme 'dracula))

(use-package bubbleberry-theme
  :disabled t
  :config
  (load-theme 'bubbleberry))

(use-package solarized-theme
  :init
  :disabled t
  (setq solarized-distinct-fringe-background nil)
  (setq solarized-use-less-bold t)
  (setq solarized-use-variable-pitch nil)
  (setq x-underline-at-descent-line t)
  :config
  (load-theme 'solarized-dark))

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

(provide 'init-themes)
