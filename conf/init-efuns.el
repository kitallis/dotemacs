(defun kg/split-below-and-move ()
  (interactive)
  (split-window-below)
  (other-window 1))

(defun kg/split-right-and-move ()
  (interactive)
  (split-window-right)
  (other-window 1))

(defun kg/beginning-of-line-dwim ()
  "Toggle between moving point to the first non-whitespace character, and the start of the line."
  (interactive)
  (let ((start-position (point)))
    ;; Move to the first non-whitespace character.
    (back-to-indentation)

    ;; If we haven't moved position, go to start of the line.
    (when (= (point) start-position)
      (move-beginning-of-line nil))))

(defun kg/duplicate-start-of-line-or-region ()
  "Duplicate start of line or region."
  (interactive)
  (if mark-active
      (kg/duplicate-region)
    (kg/duplicate-start-of-line)))

(defun kg/duplicate-start-of-line ()
  "Duplicate start of line."
  (let ((text (buffer-substring (point)
                                (beginning-of-thing 'line))))
    (forward-line)
    (push-mark)
    (insert text)
    (open-line 1)))

(defun kg/duplicate-region ()
  "Duplicate start of region."
  (let* ((end (region-end))
         (text (buffer-substring (region-beginning)
                                 end)))
    (goto-char end)
    (insert text)
    (push-mark end)
    (setq deactivate-mark nil)
    (exchange-point-and-mark)))

(defun kg/set-fringe-background ()
  "Set the fringe background to the same color as the regular background."
  (interactive)
  (custom-set-faces
   `(fringe ((t (:background ,(face-background 'default)))))))

(add-hook 'after-init-hook #'kg/set-fringe-background)

(defun kg/reset-ui ()
  "Reset some UI components after changing a theme."
  (interactive)
  ;; (fringe-mode 10)
  (fringe-mode '(0 . 0))
  (kg/set-fringe-background)
  (setq linum-format "%5d "))

(defun kg/rename-this-buffer-and-file ()
  "Rename current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (cond ((get-buffer new-name)
               (error "A buffer named '%s' already exists!" new-name))
              (t
               (rename-file filename new-name 1)
               (rename-buffer new-name)
               (set-visited-file-name new-name)
               (set-buffer-modified-p nil)
               (message "File '%s' successfully renamed to '%s'"
			name
			(file-name-nondirectory new-name))))))))

(defun kg/delete-this-buffer-and-file ()
  "Remove file connected to current buffer and kill buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (when (yes-or-no-p "Are you sure you want to remove this file? ")
        (delete-file filename)
        (kill-buffer buffer)
        (message "File '%s' successfully removed" filename)))))

(defun kg/search-marked-region-if-available (start end)
  "Pre-fill counsel-rg with marked region if available."
    (interactive "r")
    (if (use-region-p)
        (let ((regionp (buffer-substring start end)))
          (counsel-rg regionp))
      (counsel-rg)))

(defun kg/show-user-config ()
  (interactive)
  (find-file "~/.emacs.d/init-user.el"))

(defun kg/toggle-maximize-buffer () "Maximize buffer"
  (interactive)
  (if (= 1 (length (window-list)))
      (jump-to-register '_)
    (progn
      (window-configuration-to-register '_)
      (delete-other-windows))))

(defun kg/isearch-query-replace-symbol-at-point ()
    "Run `query-replace-regexp' for the symbol at point."
    (interactive)
    (isearch-forward-symbol-at-point)
    (isearch-query-replace-regexp))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ADDED BY STEVE TEH GREAT ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defconst me/cache-directory
  (expand-file-name "~/.emacs.d/.cache/")
  "Directory where all cache files should be saved")

(defun me/cache-concat (name)
  "Return the absolute path of NAME under `me/cache-directory'."
  (let* ((directory (file-name-as-directory me/cache-directory))
         (path (convert-standard-filename (concat directory name))))
    (make-directory (file-name-directory path) t)
    path))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; END O XTRA GARBAGE STUFF ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'init-efuns)
