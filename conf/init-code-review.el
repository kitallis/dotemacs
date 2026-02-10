(use-package gptel
  :ensure t
  :config
  (setq gptel-model "claude-sonnet-4-5-20250929"
        gptel-backend (gptel-make-anthropic "Claude"
                        :stream t
                        :key (getenv "ANTHROPIC_API_KEY"))))

(defvar my/code-review-system-prompt
  "You are a precise code reviewer. Your job is to identify substantive issues \
in code diffs: bugs, logic errors, edge cases, security vulnerabilities, race \
conditions, and correctness problems.

Rules:
- Do NOT provide general feedback, summaries, explanations of changes, or praise.
- Focus solely on specific, objective issues based on the diff context.
- Do not make broad comments about potential system impacts or question intentions.
- If a file has no issues, respond with LGTM! for that file.
- For code snippets, use org-mode src blocks: #+begin_src LANG ... #+end_src
- For fixes, use #+begin_src diff blocks marking changes with + or -.
- Do not annotate code snippets with line numbers inside src blocks.
- Use org-mode markup throughout: * for headings, = for inline code, - for lists."
  "System prompt for AI code review.")

(defvar my/code-review-prompt-template
  "* Review Instructions

Input: Diff hunks annotated with line numbers (new code) and old hunks (replaced code).
Task: Review the new hunks for substantive issues and respond with comments.
Output: Review comments in org-mode format, grouped by file.

For each file with issues, use this format:

* file: path/to/file
** Lines START-END: Short issue title
Description of the issue.
#+begin_src diff
- problematic line
+ suggested fix
#+end_src

For files with no issues:
* file: path/to/file
LGTM!

* Example

Given this diff:
--- a/utils.py
+++ b/utils.py
@@ -18,6 +18,8 @@
 def divide(x, y):
     return x / y

+def add(x, y):
+    retrn x + y
+
 def subtract(x, y):

Your response should be:
* file: utils.py
** Lines 21-22: Typo causes NameError
=retrn= is not valid Python.
#+begin_src diff
-    retrn x + y
+    return x + y
#+end_src

* Diff to review

%s"
  "Prompt template for AI code review. Expects a single %s for the diff.")

(defun my/symbolic-ref ()
  (magit-git-string "symbolic-ref" "refs/remotes/origin/HEADS"))

(defun my/merge-base (A B)
  (magit-git-string "merge-base" A B))

(defun my/default-remote-ref ()
  (string-remove-prefix "refs/remotes/origin/" (my/symbolic-ref)))

(defun my/default-common-ref ()
  (seq-find #'magit-branch-p '("main" "master")))

(defun my/code-review--default-branch ()
  "Tries origin/HEAD first, then falls back to common names."
  (or (my/default-remote-ref)
      (my/default-common-ref)))

(defun my/code-review--branch-diff ()
  "Return (DIFF-STRING . DESCRIPTION) for the overall branch context, or nil.
On a feature branch: diff against the default branch merge-base.
On the default branch: diff against the remote tracking branch."
  (let* ((current (magit-get-current-branch))
         (default-branch (my/code-review--default-branch)))
    (cond
     ;; Feature branch: diff from merge-base with default branch
     ((and current default-branch (not (string= current default-branch)))
      (let* ((merge-base (my/merge-base default current))
             (base (or merge-base default-branch))
             (diff (with-temp-buffer
                     (magit-git-insert "diff" base "HEAD")
                     (buffer-string))))
        (when (and diff (not (string-empty-p diff)))
          (cons diff (format "branch %s vs %s" current default-branch)))))

     ;; Default branch: diff against remote
     ((and current (magit-get-upstream-branch current))
      (let ((diff (with-temp-buffer
                    (magit-git-insert "diff"
                                      (magit-get-upstream-branch current)
                                      "HEAD")
                    (buffer-string))))
        (when (and diff (not (string-empty-p diff)))
          (cons diff (format "unpushed changes on %s" current))))))))

(defun my/code-review--staged-diff ()
  "Return (DIFF-STRING . DESCRIPTION) for staged changes, or nil."
  (let ((diff (with-temp-buffer
                (magit-git-insert "diff" "--cached")
                (buffer-string))))
    (when (and diff (not (string-empty-p diff)))
      (cons diff "staged changes"))))

(defun my/code-review--request (buf prompt)
  "Send PROMPT to gptel, streaming into BUF at point-max."
  (gptel-request prompt
    :buffer buf
    :position (with-current-buffer buf (point-max))
    :system my/code-review-system-prompt))

(defun my/magit-ai-review ()
  "Review changes with AI.
Includes two sections when available:
- Overall: all branch changes vs default branch (or unpushed on default)
- Staged: currently staged changes
Falls back to staged-only if no branch context is available."
  (interactive)
  (let ((branch-info (my/code-review--branch-diff))
        (staged-info (my/code-review--staged-diff)))
    (if (and (null branch-info) (null staged-info))
        (message "No changes to review")
      (let ((buf (get-buffer-create "*AI Code Review*")))
        (with-current-buffer buf
          (erase-buffer)
          (if (fboundp 'org-mode) (org-mode) (fundamental-mode)))

        (pop-to-buffer buf)

        (when branch-info
          (with-current-buffer buf
            (goto-char (point-max))
            (insert (format "* Overall: %s\nReviewing...\n\n" (cdr branch-info))))
          (my/code-review--request
           buf (format my/code-review-prompt-template (car branch-info))))

        (when staged-info
          (with-current-buffer buf
            (goto-char (point-max))
            (insert "\n* Staged changes\nReviewing...\n\n"))
          (my/code-review--request
           buf (format my/code-review-prompt-template (car staged-info))))))))

;; Bind it in Magit
(with-eval-after-load 'magit
  (transient-append-suffix 'magit-diff "d"
    '("R" "AI Review" my/magit-ai-review)))

(provide 'init-code-review)
