;;; package --- Summary:
;;; Commentary:
;;; Code:

;;;###autoload
(defun my/bootstrap--tools ()
	(message "Bootstrap: tools-core")
	(my/bootstrap--tools-flycheck)
	(my/bootstrap--tools-project)
	(my/bootstrap--tools-search))

(provide 'tools)
;;; tools.el ends here
