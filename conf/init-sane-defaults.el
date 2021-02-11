;; Automatically reload files when they change on disk
(global-auto-revert-mode 1)
(setq auto-revert-verbose nil)

;; Use y/n instead of full yes/no for confirmation messages
(fset 'yes-or-no-p 'y-or-n-p)

;; Use spaces instead of tabs
(setq tab-width 2)
(setq js-indent-level 2)
(setq css-indent-offset 2)
(setq c-basic-offset 2)
(setq-default indent-tabs-mode nil)
(setq-default c-basic-offset 2)
(setq-default tab-width 2)
(setq-default c-basic-indent 2)

;; Type over selected text
(delete-selection-mode 1)

;; Kill whole line
(global-set-key (kbd "s-<backspace>") 'kill-whole-line)

;; Use Cmd for movement
(global-set-key (kbd "s-<right>") (kbd "C-e"))  ;; End of line
(global-set-key (kbd "s-<left>") (kbd "C-a"))   ;; Beginning of line

;; Kills the current buffer without displaying the annoying menu.
;; A confirmation will be asked for, if the buffer has been modified
(global-set-key (kbd "C-x k") 'kill-this-buffer)

;; Remove trailing whitespace before saving
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Add ability to shift between buffers using shift+arrow keys.
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

;; On OS X, an Emacs instance started from the graphical user
;; interface will have a different environment than a shell in a
;; terminal window, because OS X does not run a shell during the
;; login. Obviously this will lead to unexpected results when
;; calling external utilities like make from Emacs.
;; This library works around this problem by copying important
;; environment variables from the user's shell.
;; https://github.com/purcell/exec-path-from-shell
(if *is-a-mac*
    (use-package exec-path-from-shell
      :config
      (exec-path-from-shell-initialize)))

(provide 'init-sane-defaults)
