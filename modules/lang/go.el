;;; package --- Summary:
;;; Commentary:
;;; Code:

;;;###autoload
(defun my/bootstrap--lang-go ()
	(message "Boostrap: lang-go")

	(use-package go-mode
		:init
			(which-key-add-major-mode-key-based-replacements 'go-mode
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

			(evil-leader/set-key-for-mode 'go-mode
				"mfd" 'lsp-ui-peek-find-definitions
				"mfi" 'lsp-ui-peek-find-implementation
				"mfs" 'lsp-ui-peek-find-workspace-symbol
				"mfu" 'lsp-ui-peek-find-references

				"mra" 'lsp-execute-code-action
				"mrr" 'lsp-rename

				"m=f" 'lsp-format-buffer
				"m=r" 'lsp-format-region))
		)

(provide 'go)
;;; go.el ends here
