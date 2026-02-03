(use-package magit
  :bind ("C-x g" . 'magit-status)
  :config
  (setq magit-set-upstream-on-push 'askifnotset))

(use-package gptel
  :ensure t
  :config
  ;; Configure for Claude (or your preferred provider)
  (setq gptel-model "claude-sonnet-4-5-20250929"
        gptel-backend (gptel-make-anthropic "Claude"
                        :stream t
                        :key (getenv "ANTHROPIC_API_KEY"))))

(defun my/magit-ai-review-staged ()
  "Review staged changes with AI."
  (interactive)
  (let ((diff (with-temp-buffer
                (magit-git-insert "diff" "--cached")
                (buffer-string))))
    (if (or (null diff) (string-empty-p diff))
        (message "No staged changes to review")
      (let ((prompt (format "Review this diff for bugs, edge cases, style issues, and suggestions:\n\n```diff\n%s\n```" diff))
            (buf (get-buffer-create "*AI Code Review*")))
        (with-current-buffer buf
          (erase-buffer)
          (org-mode)
          (insert "## Reviewing staged changes...\n\n"))
        (pop-to-buffer buf)
        (gptel-request prompt
          :buffer buf
          :position (point-max))))))

;; Bind it in Magit
(with-eval-after-load 'magit
  (transient-append-suffix 'magit-diff "d"
    '("R" "AI Review staged" my/magit-ai-review-staged)))

(add-hook 'magit-status-sections-hook 'magit-insert-worktrees)

(provide 'init-git)
