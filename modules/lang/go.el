;;; package --- Summary:
;;; Commentary:
;;; Code:

;;;###autoload
(defun my/bootstrap--lang-go ()
	(message "Boostrap: lang-go")

	(use-package go-mode)

	(use-package lsp-go
		:after (go-mode)
		:custom
			(lsp-go-executable-path "/Users/justen/go/bin/go-langserver")
			(lsp-go-gocode-completion-enabled t)
		:hook (go-mode . lsp-go-enable)
	    :config (add-to-list 'lsp-go-language-server-flags "-diagnostics"))

	)
(provide 'go)
;;; go.el ends here
