;;; package --- Summary:
;;; Commentary:
;;; Code:

(declare-function company-grab-symbol "company" ())
(declare-function flycheck-add-next-checker "flycheck" (checker next &optional append))

(defun my/company-transformer (candidates)
  (let ((completion-ignore-case t))
	(message "%s" (all-completions (company-grab-symbol) candidates))
    (all-completions (company-grab-symbol) candidates)))

(defun my/company-js-hook nil
  (message "company hook")
  (make-local-variable 'company-transformers)
  (push 'my/company-transformer company-transformers))


(defun setup-tide-mode ()
	(interactive)
	(message "starting tide")
	(tide-setup)
	(flycheck-add-next-checker 'typescript-tide 'javascript-eslint 'append)
	)

(use-package tide
  :ensure t
  :after (typescript-mode flycheck)
  :hook (typescript-mode . setup-tide-mode)
    (js-jsx-mode . setup-tide-mode)
	)

;;;###autoload
(defun my/bootstrap--lang-javascript ()
	(message "Boostrap: lang-javascript")

	(use-package add-node-modules-path
		:hook (js-mode . add-node-modules-path)
		      (js2-mode . add-node-modules-path))


	(which-key-add-major-mode-key-based-replacements 'js-mode
		"<SPC> mf" "find"
		"<SPC> mfd" "definitions"
		"<SPC> mfi" "implementations"
		"<SPC> mfs" "symbols"
		"<SPC> mfu" "usages"
		"<SPC> mr" "refactor"
		"<SPC> mra" "code action"
		"<SPC> mrr" "rename"
		"<SPC> m=" "format"
		"<SPC> m=f" "file"
		"<SPC> m=r" "region"
		)

	(evil-leader/set-key-for-mode 'js-mode
		"mfd" 'lsp-ui-peek-find-definitions
		"mfi" 'lsp-ui-peek-find-implementation
		"mfs" 'lsp-ui-peek-find-workspace-symbol
		"mfu" 'lsp-ui-peek-find-references
		"mrr" 'lsp-rename
		"mra" 'lsp-execute-code-action
		"m=f" 'lsp-format-buffer
		"m=r" 'lsp-format-region)

	(which-key-add-major-mode-key-based-replacements 'js-jsx-mode
		"<SPC> mf" "find"
		"<SPC> mfd" "definitions"
		"<SPC> mfi" "implementations"
		"<SPC> mfs" "symbols"
		"<SPC> mfu" "usages"
		"<SPC> mr" "refactor"
		"<SPC> mra" "code action"
		"<SPC> mrr" "rename"
		"<SPC> m=" "format"
		"<SPC> m=f" "file"
		"<SPC> m=r" "region"
		)

	(evil-leader/set-key-for-mode 'js-jsx-mode
		"mfd" 'lsp-ui-peek-find-definitions
		"mfi" 'lsp-ui-peek-find-implementation
		"mfs" 'lsp-ui-peek-find-workspace-symbol
		"mfu" 'lsp-ui-peek-find-references
		"mrr" 'lsp-rename
		"mra" 'lsp-execute-code-action
		"m=f" 'lsp-format-buffer
		"m=r" 'lsp-format-region)

	(use-package typescript-mode
		:init
			(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))
			(add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-mode))
			(which-key-add-major-mode-key-based-replacements 'typescript-mode
				"<SPC> mf" "find"
				"<SPC> mfd" "definitions"
				"<SPC> mfi" "implementations"
				"<SPC> mfs" "symbols"
				"<SPC> mfu" "usages"
				"<SPC> mr" "refactor"
				"<SPC> mra" "code action"
				"<SPC> mrr" "rename"
				"<SPC> m=" "format"
				"<SPC> m=f" "file"
				"<SPC> m=r" "region"
				)

			(evil-leader/set-key-for-mode 'typescript-mode
				"mfd" 'lsp-ui-peek-find-definitions
				"mfi" 'lsp-ui-peek-find-implementation
				"mfs" 'lsp-ui-peek-find-workspace-symbol
				"mfu" 'lsp-ui-peek-find-references
				"mrr" 'lsp-rename
				"mra" 'lsp-execute-code-action
				"m=f" 'lsp-format-buffer
				"m=r" 'lsp-format-region))

	(use-package graphql-mode)

	(use-package js2-mode
		:custom
			(js2-mode-show-strict-warnings nil)
			(js2-mode-show-parse-errors nil)
			(js2-highlight-external-variables nil)
		:init
			(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
			(add-to-list 'auto-mode-alist '("\\.mjs\\'" . js2-mode))
		(add-hook 'js-mode-hook 'my/company-js-hook)

		(which-key-add-major-mode-key-based-replacements 'js2-mode
			"<SPC> mf" "find"
			"<SPC> mfd" "definitions"
			"<SPC> mfi" "implementations"
			"<SPC> mfs" "symbols"
			"<SPC> mfu" "usages"
			"<SPC> mr" "refactor"
			"<SPC> mra" "code action"
			"<SPC> mrr" "rename"
			"<SPC> m=" "format"
			"<SPC> m=f" "file"
			"<SPC> m=r" "region"
			)

	(evil-leader/set-key-for-mode 'js2-mode
		"mfd" 'lsp-ui-peek-find-definitions
		"mfi" 'lsp-ui-peek-find-implementation
		"mfs" 'lsp-ui-peek-find-workspace-symbol
		"mfu" 'lsp-ui-peek-find-references
		"mrr" 'lsp-rename
		"mra" 'lsp-execute-code-action
		"m=f" 'lsp-format-buffer
		"m=r" 'lsp-format-region))

	(use-package prettier-js
		:custom
		(prettier-js-show-errors nil)
		:hook (js2-mode . prettier-js-mode)
			  (js-jsx-mode . prettier-js-mode)
			  (typescript-mode . prettier-js-mode)
			  (js-mode . prettier-js-mode))

	)
(provide 'javascript)
;;; javascript.el ends here
