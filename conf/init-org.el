(use-package org
  :config
  (setq org-directory "~/org-notes"
        org-default-notes-file (concat org-directory "/notes.txt")
        org-export-coding-system 'utf-8
        org-ellipsis " ▼ "
        org-startup-indented nil
        org-src-fontify-natively t
        org-src-preserve-indentation t)
  (setq org-capture-templates
        '(("l" "Add a link to the linklog" entry
           (file+olp+datetree (lambda () (concat org-directory "/linklog.org")))
           "**** %?")))
  (add-hook 'org-mode-hook
            (lambda () (when (fboundp 'org-mac-grab-link)
                         (load-file "~/.emacs.d/org-mac-link.el")))))

(setq org-todo-keywords
      '((sequence "TODO" "DOING" "|" "DONE")))

(use-package org-bullets
  :after org
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(provide 'init-org)
