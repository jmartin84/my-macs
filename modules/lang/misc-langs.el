;;; package --- Summary:
;;; Commentary:
;;; Code:

;;;###autoload
(defun my/bootstrap--lang-misc ()
	(message "Boostrap: lang-misc")
	(use-package json-mode
		:init
		(add-to-list 'auto-mode-alist '("\\.json\\'" . json-mode)))

	(use-package yaml-mode
		:init (add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-mode)))

	(use-package terraform-mode)

	(use-package company-terraform
		:init (company-terraform-init))

	(use-package csv-mode
		:init (add-to-list 'auto-mode-alist '("\\.csv\\'" . csv-mode)))

	(use-package dockerfile-mode
		:init (add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode)))

 )

(provide 'misc-langs)
;;; misc-langs.el ends here
