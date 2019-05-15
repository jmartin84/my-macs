;;; package --- Summary:
;;; Commentary:
;;; Code:

;;;###autoload
(defun my/bootstrap--ui-mode-line ()
	(message "Boostrap: ui-mode-line")
	(use-package doom-modeline
		:hook (after-init . doom-modeline-init)))

(provide 'ui-mode-line)
;;; mode-line.el ends here
