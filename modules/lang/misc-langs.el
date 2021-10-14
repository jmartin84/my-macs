;;; package --- Summary:
;;; Commentary:
;;; Code:

;;;###autoload
(defun my/bootstrap--lang-misc ()
	(message "Boostrap: lang-misc")
	(use-package graphql-mode)

	(use-package json-mode
		:mode "\\.json\\'")

	(use-package yaml-mode
		:mode "\\.yaml\\'")

	(use-package terraform-mode)

	(use-package company-terraform
		:init (company-terraform-init))

	(use-package csv-mode
		:mode "\\.csv\\'")

	(use-package dockerfile-mode
		:init (add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode)))

 )

(provide 'misc-langs)
;;; misc-langs.el ends here
