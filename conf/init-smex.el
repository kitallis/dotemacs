;; M-x with recently/frequently used commands
(use-package smex
  :config
  (global-set-key (kbd "M-x") 'smex)
  (global-set-key (kbd "s-x") 'smex))

(provide 'init-smex)
