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

	(use-package add-node-modules-path
		:hook (js2-mode . add-node-modules-path))

	(use-package add-node-modules-path
		:hook (js-mode . add-node-modules-path))

	(use-package typescript-mode
		:init
			(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))
			(which-key-add-major-mode-key-based-replacements 'typescript-mode
				"<SPC> mf" "find"
				"<SPC> mfd" "definitions"
				"<SPC> mfi" "implementations"
				"<SPC> mfs" "symbols"
				"<SPC> mfu" "usages"
				"<SPC> mr" "refactor"
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

		"m=f" 'lsp-format-buffer
		"m=r" 'lsp-format-region))

	(use-package rjsx-mode
		:after (js2-mode)
		:init
		(add-hook 'rjsx-mode-hook
			(lambda ()
				(add-hook 'before-save-hook
						(lambda ()
							(if indent-tabs-mode
								(tabify (point-min) (point-max))
							(untabify (point-min) (point-max))
							))
						nil
						t)))

		(add-to-list 'auto-mode-alist '("\\.tsx\\'" . rjsx-mode))

		(which-key-add-major-mode-key-based-replacements 'rjsx-mode
				"<SPC> mf" "find"
				"<SPC> mfd" "definitions"
				"<SPC> mfi" "implementations"
				"<SPC> mfs" "symbols"
				"<SPC> mfu" "usages"
				"<SPC> mr" "refactor"
				"<SPC> mrr" "rename"
				"<SPC> m=" "format"
				"<SPC> m=f" "file"
				"<SPC> m=r" "region"
				)

		(evil-leader/set-key-for-mode 'rjsx-mode
			"mfd" 'lsp-ui-peek-find-definitions
			"mfi" 'lsp-ui-peek-find-implementation
			"mfs" 'lsp-ui-peek-find-workspace-symbol
			"mfu" 'lsp-ui-peek-find-references

			"mrr" 'lsp-rename

			"m=f" 'lsp-format-buffer
			"m=r" 'lsp-format-region))

	;; (use-package lsp-typescript
	;; 	:disabled t
	;; 	:after (lsp-mode)
	;; 	:hook
	;; 		(rjsx-mode . lsp-typescript-enable)
	;; 		(js2-mode . lsp-typescript-enable))

	;; (use-package lsp-javascript-typescript
	;; 	:disabled t
	;; 	:after (lsp-mode)
	;; 	:hook
	;; 		(rjsx-mode . lsp-javascript-typescript-enable)
	;; 		(js2-mode . lsp-javascript-typescript-enable))

	;; (use-package company-tern
	;; 	:after (company)
	;; 	:init (add-to-list 'company-backends 'company-tern)
	;; 	:hook
	;; 		(rjsx-mode . tern-mode)
	;; 		(js-mode . tern-mode)
	;; 		(js2-mode . tern-mode))

	)
(provide 'javascript)
;;; javascript.el ends here
