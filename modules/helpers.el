;;; package --- Summary
;;; Commentary:
;;; Code:

;;;###autoload
(defun my/log-package-init (packageName) (message (format "Initializing package: %s" packageName)))

;;;###autoload
(defun my/not-implemented () (message "**NOT YET IMPLEMENTED**"))

;;;###autoload
(defun my/open-messages-buffer () (interactive) (switch-to-buffer "*Messages*"))

;;;###autoload
(defun my/restart-and-debug () (interactive) (restart-emacs '("--debug-init")))
