(use-package lsp-mode
  :hook ((clojure-mode . lsp)
         (clojurec-mode . lsp)
         (clojurescript-mode . lsp)
         (sh-mode . lsp))

  :config
  ;; add paths to your local installation of project mgmt tools, like lein
  (setenv "PATH" (concat "/usr/local/bin" path-separator (getenv "PATH")))
  (dolist (m '(clojure-mode
               clojurec-mode
               clojurescript-mode
               clojurex-mode))
    (add-to-list 'lsp-language-id-configuration `(,m . "clojure")))

  ;; Optional: In case `clojure-lsp` is not in your PATH
  (setq lsp-clojure-server-command '("bash" "-c" "clojure-lsp"))

  :custom
  ;; turn this on to capture client/server comms before
  ;; submitting bug reports with `lsp-workspace-show-log`
  ;; (lsp-log-io t)

  (lsp-auto-guess-root t)
  (lsp-eldoc-enable-hover nil)
  (lsp-enable-indentation nil)
  (lsp-enable-folding nil)
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-idle-delay .01)
  (lsp-keymap-prefix nil)
  (lsp-session-file (me/cache-concat "lsp/session.eld")))


(use-package lsp-ui
  :commands lsp-ui-mode)

(provide 'init-lsp)
