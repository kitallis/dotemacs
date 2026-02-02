(use-package magit
  :bind ("C-x g" . 'magit-status)
  :config
  (setq magit-set-upstream-on-push 'askifnotset))

(add-hook 'magit-status-sections-hook 'magit-insert-worktrees)

(provide 'init-git)
