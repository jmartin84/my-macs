;;; package --- Summary
;;; Commentary:
;;; Code:

;;;###autoload
(defun my/bootstrap--ui-linenums ()
	(message "Bootstrap: ui-linenums")
	(setq display-line-numbers-type 'relative)
	(add-hook 'prog-mode-hook 'display-line-numbers-mode)
	(add-hook 'text-mode-hook 'display-line-numbers-mode))

(provide 'ui-linenums)
;;; linenums.el ends here
