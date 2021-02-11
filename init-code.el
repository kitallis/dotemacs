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

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "pandoc")
  :config
  (add-hook 'gfm-mode-hook 'linum-mode)
  (add-hook 'markdown-mode-hook 'linum-mode))

(use-package smartparens
  :init (add-hook 'markdown-mode-hook 'smartparens-mode))

(use-package elixir-mode)

(use-package alchemist)

(use-package yaml-mode)

(use-package json-mode)

(use-package k8s-mode
  :hook (k8s-mode . yas-minor-mode))

(use-package dockerfile-mode
  :config
  (require 'dockerfile-mode)
  (add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode)))

(use-package lsp-mode
  :hook ((clojure-mode . lsp)
         (clojurec-mode . lsp)
         (clojurescript-mode . lsp)
         (web-mode . lsp-deferred))
  :config
  ;; add paths to your local installation of project mgmt tools, like lein
  (setenv "PATH" (concat "/usr/local/bin" path-separator (getenv "PATH")))
  (dolist (m '(clojure-mode
               clojurec-mode
               clojurescript-mode
               web-mode
               clojurex-mode))
    (add-to-list 'lsp-language-id-configuration `(,m . "clojure")))

  ;; Optional: In case `clojure-lsp` is not in your PATH
  (setq lsp-clojure-server-command '("bash" "-c" "clojure-lsp"))

  :custom
  (lsp-auto-guess-root t)
  (lsp-eldoc-enable-hover nil)
  (lsp-enable-file-watchers nil)
  (lsp-enable-folding nil)
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-idle-delay .01)
  (lsp-keymap-prefix nil)
  (lsp-session-file (me/cache-concat "lsp/session.eld")))

(use-package lsp-ui
  :commands lsp-ui-mode)
