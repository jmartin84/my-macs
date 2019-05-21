;;; package --- Summary:
;;; Commentary:
;;; Code:

;;;###autoload
(defun my/bootstrap--tools-utils ()
	(message "Bootstrap: utils")

	;; restart emacs
	(use-package restart-emacs :pin melpa-stable)

	(use-package editorconfig
		:ensure t
		:config
		(editorconfig-mode 1))
	)

(provide 'utils)
;;; tools.el ends here
