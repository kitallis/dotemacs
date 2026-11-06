;;; early-init.el --- Early init -*- lexical-binding: t; -*-

;; Emacs 31.1 from emacsformacosx.com stamps -mmacosx-version-min=18.0,
;; a macOS version that never existed (Apple jumped 15 -> 26). Newer
;; clang rejects it, breaking all native-comp. Override to a real target.
(setq native-comp-driver-options '("-mmacosx-version-min=26.0"))

(setq package-enable-at-startup nil)

;; Emacs 31.1 on macOS crashes with face_for_font recursion when
;; display-delayed-warnings renders ⛔ in the *Warnings* buffer during
;; startup. Suppress the popup — warnings still land in the buffer.
(setq warning-minimum-level :error)

(provide 'early-init)
;;; early-init.el ends here
