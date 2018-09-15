;;; package --- Summary:
;;; Commentary:
;;; Code:

(defun my/company-transformer (candidates)
  (let ((completion-ignore-case t))
	(message "%s" (all-completions (company-grab-symbol) candidates))
    (all-completions (company-grab-symbol) candidates)))

(defun my/company-js-hook nil
  (message "company hook")
  (make-local-variable 'company-transformers)
  (push 'my/company-transformer company-transformers))

;;;###autoload
(defun my/bootstrap--lang-javascript ()
	(message "Boostrap: lang-javascript")

	(use-package add-node-modules-path)

	(use-package js2-mode
		:custom
			(js2-mode-show-strict-warnings nil)
			(js2-mode-show-parse-errors nil)
			(js2-highlight-external-variables nil)
		:init
			(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
		:config
			(add-hook 'js-mode-hook #'my/company-js-hook))

	(use-package rjsx-mode
		:after (js2-mode))

	(use-package lsp-javascript-typescript
		:after (lsp-mode)
		:hook
			(rjsx-mode . lsp-javascript-typescript-enable)
			(js-mode . lsp-javascript-typescript-enable))

	)
(provide 'javascript)
;;; javascript.el ends here
