;; Install and setup company-mode for autocompletion
(use-package company
  :bind (("C-p" . company-select-previous)
         ("C-n" . company-select-next))
  :init
  (add-hook 'prog-mode-hook 'company-mode)
  :config
  (global-company-mode)
  (setq company-tooltip-limit 10)
  (setq company-idle-delay 0.2)
  (setq company-echo-delay 0)
  (setq company-minimum-prefix-length 2)
  (setq company-require-match nil)
  (setq company-selection-wrap-around t)
  (setq company-tooltip-align-annotations t)
  (setq company-tooltip-flip-when-above nil)
  (setq company-dabbrev-ignore-case nil
        company-dabbrev-downcase nil)
  ;; weight by frequency
  (setq company-transformers '(company-sort-by-occurrence)))

(provide 'init-company)
