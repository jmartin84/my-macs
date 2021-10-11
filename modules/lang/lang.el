;;; package --- Summary:
;;; Commentary:
;;; Code:

;;;###autoload
(defun my/bootstrap--lang ()
	(message "Bootstrap: lang-core")
	(my/bootstrap--lang-csharp)
	(my/bootstrap--lang-go)
	(my/bootstrap--lang-javascript)
	(my/bootstrap--lang-misc))

(provide 'lang)
;;; lang.el ends here
