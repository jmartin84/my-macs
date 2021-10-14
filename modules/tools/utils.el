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
		:hook
			(prog-mode . editorconfig-mode)
			(text-mode . editorconfig-mode))

	(use-package hydra)
	)

(provide 'utils)
;;; tools.el ends here
