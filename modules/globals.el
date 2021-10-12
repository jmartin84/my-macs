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

;; functions


(provide 'globals)
;;; globals.el ends here
