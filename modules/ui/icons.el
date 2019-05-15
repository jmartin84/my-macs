;;; package --- Summary:
;;; Commentary:
;;; Code:

;;;###autoload
(defun my/bootstrap--ui-icons ()
	(message "Boostrap: ui-icons")
    (add-to-list 'load-path "~/.local/share/icons-in-terminal/")

	;; all-the-icons
	(use-package all-the-icons)
	(use-package all-the-icons-dired
		:after (all-the-icons)
		:hook (dired-mode . all-the-icons-dired-mode))

	(use-package fringe-helper))
(provide 'ui-icons)
;;; icons.el ends here
