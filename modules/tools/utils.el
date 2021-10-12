;;; package --- Summary:
;;; Commentary:
;;; Code:

;;;###autoload
(defun my/bootstrap--tools-utils ()
	(message "Bootstrap: utils")

	;; restart emacs
	(use-package restart-emacs )

	(use-package editorconfig
		:ensure t
		:config
		(editorconfig-mode 1))

	(use-package hydra)
	)

(provide 'utils)
;;; tools.el ends here
