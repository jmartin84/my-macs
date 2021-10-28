;;; package --- Summary:
;;; Commentary:
;;; Code:

(declare-function my/bootstrap--lang-csharp "csharp" ())
(declare-function my/bootstrap--lang-go "go" ())
(declare-function my/bootstrap--lang-javascript "javascript" ())
(declare-function my/bootstrap--lang-misc "misc-langs" ())

;;;###autoload
(defun my/bootstrap--lang ()
	(message "Bootstrap: lang-core")
	(my/bootstrap--lang-csharp)
	(my/bootstrap--lang-go)
	(my/bootstrap--lang-javascript)
	(my/bootstrap--lang-python)
	(my/bootstrap--lang-misc))

(provide 'lang)
;;; lang.el ends here
