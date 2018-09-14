;;; package --- Summary:
;;; Commentary:
;;; Code:

;;;###autoload
(defun my/bootstrap--ui ()
	(message "Bootstrap: ui-core")
	(set-default 'truncate-lines t)
	(scroll-bar-mode -1)
	(tool-bar-mode -1)
	(setq default-frame-alist '((ns-transparent-titlebar . t) (ns-appearance . 'nil)))
	(my/bootstrap--ui-font)
	(my/bootstrap--ui-icons)
	(my/bootstrap--ui-linenums)
	(my/bootstrap--ui-theme)
	(my/bootstrap--ui-mode-line))

(provide 'ui)
;;; core.el ends here
