;;; globals.el --- Summary
;;; Commentary:
;;; Code:



;;variables
(defvar use-package-always-ensure t)

;; something is requiring evil before evil's use package
;; so we set this very very early until we figure out whats going on
;; probably whichkey
(defvar evil-want-keybinding nil)
(setq package-check-signature nil)
(setq use-package-verbose t)
(setq use-short-answers t)
(setq kill-buffer-delete-auto-save-files t)
(setq mode-line-compact 'long)

;; functions


(provide 'globals)
;;; globals.el ends here
