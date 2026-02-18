;;; init-code-review-nxt.el --- magit-hutch buffer UI -*- lexical-binding: t; -*-
;;; Parked: magit-section rendering, accept/reject, gptel request dispatch.
;;; This will be revisited once the agentic loop in init-code-review.el is solid.

(require 'init-code-review)

;;; --- Magit-section review buffer ---

(with-eval-after-load 'magit

  ;; Mode and keymaps

  (defvar-keymap my/code-review-mode-map
    :parent magit-section-mode-map
    "q" #'quit-window)

  (define-derived-mode my/code-review-mode magit-section-mode "Code-Review"
    "Major mode for displaying AI code review results."
    :interactive nil)

  ;; Magit looks up keymaps as `magit-TYPE-section-map' for section type TYPE
  (defvar-keymap magit-review-suggestion-section-map
    :parent magit-section-mode-map
    "a" #'my/review-suggestion-accept
    "x" #'my/review-suggestion-reject)

  (defvar-keymap magit-review-comment-section-map
    :parent magit-section-mode-map
    "x" #'my/review-suggestion-reject)

  ;; Display helpers

  (defun my/review--diff-line-face (line)
    "Return the face for a diff LINE based on its prefix."
    (cond
     ((string-prefix-p "+" line)  'magit-diff-added)
     ((string-prefix-p "-" line)  'magit-diff-removed)
     ((string-prefix-p "@@" line) 'magit-diff-hunk-heading)
     (t 'default)))

  (defun my/review--diff-header-p (line)
    "Return t if LINE is a diff file header (--- or +++)."
    (or (string-prefix-p "---" line)
        (string-prefix-p "+++" line)))

  (defun my/review--insert-patch-lines (patch)
    "Insert colorized diff lines from PATCH string, skipping file headers."
    (insert "\n")
    (dolist (line (split-string patch "\n"))
      (unless (my/review--diff-header-p line)
        (insert (format "    %s\n"
                        (propertize line 'font-lock-face
                                    (my/review--diff-line-face line))))))
    (insert "\n"))

  ;; Section inserters — one per type

  (defun my/review--wrap-text (text width prefix)
    "Wrap TEXT to WIDTH, prefixing each line with PREFIX."
    (with-temp-buffer
      (insert text)
      (let ((fill-column (- width (length prefix)))
            (fill-prefix prefix))
        (fill-region (point-min) (point-max))
        (goto-char (point-min))
        (insert prefix)
        (buffer-string))))

  (defun my/review--insert-desc (desc)
    "Insert DESC wrapped to 80 chars with 4-space indent."
    (insert (my/review--wrap-text desc 80 "    ") "\n"))

  (defun my/review--insert-lgtm (finding)
    "Insert a LGTM section for FINDING."
    (magit-insert-section (review-lgtm (plist-get finding :file))
      (magit-insert-heading
        (propertize (format "  ✓ %s — LGTM" (plist-get finding :file))
                    'font-lock-face 'magit-diff-file-heading))))

  (defun my/review--insert-suggestion (finding)
    "Insert a suggestion section (has a patch)."
    (magit-insert-section (review-suggestion finding t)
      (magit-insert-heading
        (propertize (format "  ⚠ %s:%s — %s"
                            (plist-get finding :file)
                            (plist-get finding :lines)
                            (plist-get finding :title))
                    'font-lock-face 'magit-diff-hunk-heading))
      (my/review--insert-desc (plist-get finding :desc))
      (my/review--insert-patch-lines (plist-get finding :patch))))

  (defun my/review--insert-comment (finding)
    "Insert a comment section (no patch). Press `x' to dismiss."
    (magit-insert-section (review-comment finding t)
      (magit-insert-heading
        (propertize (format "  💬 %s:%s — %s"
                            (plist-get finding :file)
                            (plist-get finding :lines)
                            (plist-get finding :title))
                    'font-lock-face 'magit-diff-hunk-heading))
      (my/review--insert-desc (plist-get finding :desc))))

  ;; Dispatch

  (defun my/review--insert-finding (finding)
    "Insert FINDING based on its :type."
    (pcase (plist-get finding :type)
      ('lgtm       (my/review--insert-lgtm finding))
      ('suggestion (my/review--insert-suggestion finding))
      ('comment    (my/review--insert-comment finding))))

  (defun my/review--insert-findings (findings section-label)
    "Insert FINDINGS as magit sections under SECTION-LABEL."
    (magit-insert-section (review-group section-label)
      (magit-insert-heading
        (propertize section-label 'font-lock-face 'magit-section-heading))
      (if (null findings)
          (insert "  No issues found.\n")
        (dolist (finding findings)
          (my/review--insert-finding finding)))
      (insert "\n")))

  ;; Accept / reject

  (defun my/review--git-apply-check (patch directory)
    "Dry-run PATCH with git apply --check in DIRECTORY. Return t if clean."
    (let ((default-directory directory))
      (zerop (with-temp-buffer
               (insert patch "\n")
               (call-process-region (point-min) (point-max)
                                    "git" nil t nil
                                    "apply" "--check" "-")))))

  (defun my/review--git-apply (patch directory)
    "Apply PATCH with git apply in DIRECTORY."
    (let ((default-directory directory))
      (with-temp-buffer
        (insert patch "\n")
        (call-process-region (point-min) (point-max)
                             "git" nil t nil
                             "apply" "-"))))

  (defun my/review--revert-file-buffer (file directory)
    "Revert the buffer visiting FILE under DIRECTORY, if any."
    (when-let* ((full-path (expand-file-name file directory))
                (buf (find-buffer-visiting full-path)))
      (with-current-buffer buf
        (revert-buffer t t t))))

  (defun my/review--hide-section (section buf)
    "Collapse SECTION in BUF."
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (magit-section-hide section))))

  (defun my/review-suggestion-accept ()
    "Accept the suggestion at point — apply the patch via git apply."
    (interactive)
    (when-let* ((section (magit-current-section))
                (value (oref section value))
                (patch (plist-get value :patch))
                (file (plist-get value :file))
                (lines-str (plist-get value :lines))
                (root (magit-toplevel)))
      (cond
       ((not (my/review--git-apply-check patch root))
        (message "Patch does not apply cleanly to %s — file may have changed" file))
       ((y-or-n-p (format "Apply fix to %s:%s? " file lines-str))
        (my/review--git-apply patch root)
        (my/review--revert-file-buffer file root)
        (my/review--hide-section section (current-buffer))
        (message "Applied fix to %s:%s" file lines-str)))))

  (defun my/review-suggestion-reject ()
    "Reject the suggestion at point and collapse it."
    (interactive)
    (when-let* ((section (magit-current-section)))
      (my/review--hide-section section (current-buffer))
      (message "Suggestion dismissed.")))

  ;; Request dispatch (gptel-based, to be replaced by llm agent loop)

  (defun my/review--model-cost-per-token (model key)
    "Return per-token cost for MODEL's KEY (:input-cost or :output-cost).
MODEL can be a string or symbol. Returns nil if unavailable."
    (when-let ((per-million (get (intern-soft (gptel--to-string model)) key)))
      (/ (float per-million) 1000000.0)))

  (defun my/review--estimate-cost (output-tokens)
    "Estimate cost in USD for OUTPUT-TOKENS using the current model's rate."
    (when-let ((rate (my/review--model-cost-per-token gptel-model :output-cost)))
      (* output-tokens rate)))

  (defun my/review--format-tokens (info)
    "Format token usage and estimated cost from INFO plist, or nil."
    (when-let ((output (plist-get info :output-tokens)))
      (let ((cost (my/review--estimate-cost output)))
        (if cost
            (format "(%d output tokens, ~$%.4f)" output cost)
          (format "(%d output tokens)" output)))))

  (defun my/review--replace-status (buf old-text new-text)
    "In BUF, replace OLD-TEXT with NEW-TEXT."
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (save-excursion
          (goto-char (point-min))
          (when (search-forward old-text nil t)
            (replace-match new-text t t))))))

  (defun my/code-review--request (buf prompt section-label)
    "Send PROMPT to gptel. Parse response and render sections in BUF."
    (let ((status-text (format "  Requesting review: %s..."
                               (downcase section-label))))
      (gptel-request prompt
        :callback (lambda (response info)
                    (if (not (stringp response))
                        (progn
                          (my/review--replace-status
                           buf status-text "  Review failed.")
                          (message "AI review request failed: %s"
                                   (plist-get info :status)))
                      (let ((findings (my/review--parse-response response))
                            (tokens (my/review--format-tokens info)))
                        (my/review--replace-status
                         buf status-text
                         (if tokens
                             (format "  Review complete %s" tokens)
                           "  Review complete"))
                        (with-current-buffer buf
                          (let ((inhibit-read-only t))
                            (goto-char (point-max))
                            (my/review--insert-findings
                             findings section-label))))))
        :system my/code-review-system-prompt)))

  (defun my/review--setup-buffer ()
    "Create and prepare the review buffer. Return it."
    (let ((buf (get-buffer-create "magit-hutch: code review")))
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (erase-buffer))
        (my/code-review-mode)
        (let ((inhibit-read-only t))
          (magit-insert-section (review-root)
            (insert (propertize "magit-hutch: Code Review\n"
                                'font-lock-face 'magit-section-heading)))))
      buf))

  (defun my/review--request-section (buf diff-info label-fmt)
    "Send a review request for DIFF-INFO into BUF with LABEL-FMT for display."
    (let ((section-label (format label-fmt (cdr diff-info))))
      (let ((inhibit-read-only t))
        (with-current-buffer buf
          (goto-char (point-max))
          (insert (format "  Requesting review: %s...\n"
                          (downcase section-label)))))
      (my/code-review--request
       buf
       (format my/code-review-prompt-template (car diff-info))
       section-label)))

  ;; Entry point

  (defun my/magit-ai-review ()
    "Review changes with AI.
Shows two sections when available: overall branch diff and staged changes."
    (interactive)
    (let ((branch-info (my/code-review--branch-diff))
          (staged-info (my/code-review--staged-diff)))
      (if (and (null branch-info) (null staged-info))
          (message "No changes to review")
        (let ((buf (my/review--setup-buffer)))
          (pop-to-buffer buf)
          (when branch-info
            (my/review--request-section buf branch-info "Overall: %s"))
          (when staged-info
            (my/review--request-section buf staged-info "Staged: %s"))))))

  (transient-append-suffix 'magit-diff "d"
    '("R" "AI Review" my/magit-ai-review)))

(provide 'init-code-review-nxt)
