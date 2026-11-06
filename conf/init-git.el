;;; init-git.el --- -*- lexical-binding: t; -*-
(use-package magit
  :bind ("C-x g" . 'magit-status)
  :config
  (setq magit-set-upstream-on-push 'askifnotset))

(add-hook 'magit-status-sections-hook 'magit-insert-worktrees)

(use-package gptel
  :straight t
  :defer t)

;; Default: pull hutch from MELPA. Override in init-user.el with a
;; :local-repo recipe to point at a working copy for development.
(use-package hutch
  :straight t
  :after magit
  :config
  (setq gptel-log-level 'info)
  (setq hutch-backend
        (gptel-make-anthropic "Claude"
          :key (getenv "ANTHROPIC_API_KEY")
          :models '(claude-opus-5-5)
          :request-params '(:thinking (:type "adaptive")
                            :output_config (:effort "medium"))))
  (setq gptel-cache t)
  (setq hutch-scopes          '(:staged :branch))
  (setq hutch-max-tool-rounds 40)
  (setq hutch-enabled-tools   '(read-file search-codebase surrounding-context))
  (setq hutch-trace-dir       (expand-file-name "hutch-traces" user-emacs-directory))
  (hutch-add-review-binding "H"))

(provide 'init-git)
