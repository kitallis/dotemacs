(use-package smartparens
  :init (add-hook 'markdown-mode-hook 'smartparens-mode))

;; Adds some niceties/refactoring support
(use-package clj-refactor
  :config
  (setq cljr-warn-on-eval nil)
  (add-hook 'clojure-mode-hook
            (lambda ()
              (clj-refactor-mode 1))))

;; Better syntax highlighting
(use-package clojure-mode-extra-font-locking)

;; Highlight matching parentheses
(require 'paren)
(show-paren-mode 1)
(setq show-paren-delay 1)
(set-face-background 'show-paren-match (face-background 'default))
(if (eq (frame-parameter nil 'background-mode) 'dark)
    (set-face-foreground 'show-paren-match "red")
  (set-face-foreground 'show-paren-match "black"))
(set-face-attribute 'show-paren-match nil :weight 'extra-bold)

;; Paredit makes it easier to navigate/edit s-expressions as blocks.
(use-package paredit
  :init
  (add-hook 'clojure-mode-hook 'enable-paredit-mode)
  (add-hook 'cider-repl-mode-hook 'enable-paredit-mode)
  (add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode))

;; To add some colors to those boring parens
(use-package rainbow-delimiters
  :init (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

;; Treat hyphens as a word character when transposing words
(defvar clojure-mode-with-hyphens-as-word-sep-syntax-table
  (let ((st (make-syntax-table clojure-mode-syntax-table)))
    (modify-syntax-entry ?- "w" st)
    st))

(defun transpose-words-with-hyphens (arg)
  "Treat hyphens as a word character when transposing words"
  (interactive "*p")
  (with-syntax-table clojure-mode-with-hyphens-as-word-sep-syntax-table
    (transpose-words arg)))

(define-key clojure-mode-map (kbd "M-t") 'transpose-words-with-hyphens)

;; Cider integrates a Clojure buffer with a REPL
(use-package cider
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
        nrepl-popup-stacktraces nil
        nrepl-log-messages nil
        nrepl-hide-special-buffers t
        cider-repl-use-pretty-printing t
        cider-repl-result-prefix ";; => "
        cider-repl-display-help-banner nil
        cider-font-lock-dynamically '(macro core function var))

  :config
  (add-hook 'cider-mode-hook #'eldoc-mode)
  (add-hook 'cider-repl-mode-hook #'paredit-mode)
  (add-hook 'cider-repl-mode-hook #'company-mode)
  (add-hook 'cider-repl-mode-hook
            (lambda ()
              (local-set-key (kbd "C-l") 'cider-repl-clear-buffer)))
  (add-hook 'cider-mode-hook #'company-mode)
  (add-hook 'cider-mode-hook
            (lambda ()
              (local-set-key (kbd "<C-return>") 'cider-eval-last-sexp)
              (local-set-key (kbd "C-c C-n") 'cider-eval-buffer)
              (local-set-key (kbd "C-x C-i") 'cider-inspect-last-sexp))))

(set-variable 'cider-lein-parameters "with-profile +dev repl :headless")

(provide 'init-clojure)
