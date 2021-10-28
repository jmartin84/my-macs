;;; package --- Summary:
;;; Commentary:
;;; Code:

;;;###autoload
(defun my/bootstrap--lang-python ()
	(message "Boostrap: lang-python")

	(which-key-add-major-mode-key-based-replacements 'python-mode
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

	(evil-leader/set-key-for-mode 'python-mode
		"mfd" 'lsp-ui-peek-find-definitions
		"mfi" 'lsp-ui-peek-find-implementation
		"mfs" 'lsp-ui-peek-find-workspace-symbol
		"mfu" 'lsp-ui-peek-find-references

		"mra" 'lsp-execute-code-action
		"mrr" 'lsp-rename

		"m=f" 'lsp-format-buffer
		"m=r" 'lsp-format-region)

	(use-package auto-virtualenv
		:hook (python-mode . auto-virtualenv-set-virtualenv))
	)

(provide 'python)
;;; python.el ends here
