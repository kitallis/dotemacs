(use-package magit
  :bind ("C-x g" . 'magit-status)
  :config
  (setq magit-set-upstream-on-push 'askifnotset))

(add-hook 'magit-status-sections-hook 'magit-insert-worktrees)

(use-package hutch
  :straight (:local-repo "/Users/kitallis/Code/adjaecent/magit-hutch")
  :config
  (require 'hutch-ui)
  (setq hutch-provider
        (make-llm-claude :key (getenv "ANTHROPIC_API_KEY")
                         :chat-model "claude-sonnet-4-5-20250929")))

(provide 'init-git)
